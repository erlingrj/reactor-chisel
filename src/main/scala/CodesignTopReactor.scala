package reactor

import chisel3.util._
import chisel3._
import fpgatidbits.PlatformWrapper._
import chisel3.experimental.BundleLiterals._
import scala.collection.mutable.ArrayBuffer
import fpgatidbits.PlatformWrapper._


object SwCommandType extends ChiselEnum {
  val nop, write, read, reset, terminate = Value
}

class SwCommand extends Module {
  val io = IO(new Bundle {
    val cmdIn = Input(SwCommandType())
    val cmdOut = Output(SwCommandType())
  })
  io.cmdOut := SwCommandType.nop

  val reg = RegInit(SwCommandType.nop)
  when(reg =/= io.cmdIn) {
    io.cmdOut := io.cmdIn
    reg := io.cmdIn
  }
}


abstract class SwIO extends Bundle {
  def getInputs: ArrayBuffer[SwToken[_ <: Data]]
  def getOutputs: ArrayBuffer[SwToken[_ <: Data]]
}

// FIXME: Memports and PlatformWrapperParams should be parameters
class CodesignTopReactorIO(swIOGen: () => SwIO) extends GenericAcceleratorIF(0, TesterWrapperParams) {
  val coordination = new CoordinationIO()
  val cmd = Input(UInt(SwCommandType.getWidth.W))
  val ports = swIOGen()
  val terminate = Output(Bool())
}

// Handles the present bit on the inputs. It allows the HW to consume the tokens (through a decoupled interface)
// When the SW issues a write cmd, the tokens are restacked
class SwInputPresent(nInputs: Int) extends Module {
  val io = IO(new Bundle {
    val presentIn = Vec(nInputs, Input(Bool()))
    val presentOut = Vec(nInputs, Decoupled(Bool()))
    val write = Input(Bool())
  })

  val token = RegInit(VecInit(Seq.fill(nInputs)(false.B)))
  io.presentOut zip token foreach(f => f._1.valid:= f._2)
  io.presentOut zip io.presentIn foreach (f => f._1.bits := f._2)

  for ((p,i) <- io.presentOut.zipWithIndex) {
    when (p.fire) {
      token(i) := false.B
    }
  }

  when (io.write) {
    assert(!token.asUInt.orR, "[SwInputPresent] SW wrote new inputs but the previous ones werent consumed yet")
    token.foreach(_ := true.B)
  }
}

class LtcHandlerIO(nOutputs: Int) extends Bundle {
  val outputFires = Vec(nOutputs, Input(Bool()))
  val execute = Flipped(Valid(new ExecuteIO))
  val logicalTagComplete = Output(Tag())
}

class LtcHandler(nOutputs: Int) extends Module {
  require(nOutputs > 0, "[LtcHandler] Does not support FPGA reactors without any outpits")
  val io = IO(new LtcHandlerIO(nOutputs))

  val regCurrentlyExecutingTag = RegInit(Tag(0))
  val regLogicalTagComplete = RegInit(Tag.NEVER)
  val regOutputsFired = RegInit(VecInit(Seq.fill(nOutputs)(false.B)))

  io.logicalTagComplete := regLogicalTagComplete
  for ((oFired, reg) <- io.outputFires zip regOutputsFired) {
    when (oFired) {
      assert(reg === false.B, "[LtcHandler] output fired. But it was already marked as fired")
      reg := true.B
    }
  }

  when(regOutputsFired.asUInt.andR) {
    regLogicalTagComplete := regCurrentlyExecutingTag
    regOutputsFired.foreach(_ := false.B)
  }

  when(io.execute.valid) {
    assert(!regOutputsFired.asUInt.orR, "[LtcHandler] We started executing a new tag before the previous ended")
    regCurrentlyExecutingTag := io.execute.bits.tag
  }
}



class CodesignTopReactor(mainReactorGen: () => Reactor, swIOGen: () => SwIO)
  (implicit globalCfg: GlobalReactorConfig) extends GenericAccelerator(TesterWrapperParams) {
  def numMemPorts = 0
  val io = IO(new CodesignTopReactorIO(swIOGen))
  io.signature := makeDefaultSignature()

  val cmd = Module(new SwCommand()).io
  val (cmdCast, validity) = SwCommandType.safe(io.cmd)
  assert(validity)
  cmd.cmdIn := cmdCast

  val mainReactor = Module(mainReactorGen())
  val externalIO = IO(mainReactor.externalIO.cloneType)
  externalIO <> mainReactor.externalIO

  // Create the module handling the tokens from SW to mainReactor
  val swPorts = Module(new TopLevelPorts(io.ports.cloneType, mainReactor.io.cloneType))
  swPorts.io.sw <> io.ports
  swPorts.io.mainReactor <> mainReactor.io
  swPorts.io.swCmd := cmd.cmdOut

  val triggerGen = Module(new TriggerGenerator(false, globalCfg.timeout, mainReactor))
  triggerGen.io.inputPresent := swPorts.io.inputPresent
  io.coordination.nextEventTag := triggerGen.io.nextEventTag
  triggerGen.io.tagAdvanceGrant := io.coordination.tagAdvanceGrant
  swPorts.io.execute <> triggerGen.io.execute
  io.coordination.logicalTagComplete := swPorts.io.logicalTagComplete

  // Connect the triggerGenerator to the mainReactor
  for (i <- mainReactor.triggerIO.allTimerTriggers.indices) {
    mainReactor.triggerIO.allTimerTriggers(i) <> triggerGen.io.timerTriggers(i)
  }

  // Terminate when triggerGenerator has fired the shutdown trigger AND all reactors are idle
  io.terminate := triggerGen.io.terminate && mainReactor.statusIO.idle

  // Drive the tag-validity signal
  val regTAGValid = RegInit(false.B)
  when(cmd.cmdOut === SwCommandType.write) {
    regTAGValid := true.B
  }
  when(triggerGen.io.execute.fire && EventMode.hasExternalEvent(triggerGen.io.execute.bits.eventMode.asUInt)) {
    regTAGValid := false.B
  }
  triggerGen.io.coordinationValid := regTAGValid

  // Drive the shutdownCommand signal
  val regShutdownCommand = RegInit(0.U.asTypeOf(new ShutdownCommand))
  when (cmd.cmdOut === SwCommandType.write) {
    regShutdownCommand := io.coordination.shutdownCommand
  }
  triggerGen.io.shutdownCommand := regShutdownCommand
}


class TopLevelPortsIO(swPorts: SwIO, mainReactorPorts: ReactorIO) extends Bundle {
  val swCmd = Input(SwCommandType())
  val sw = swPorts
  val mainReactor = Flipped(mainReactorPorts)
  val execute = Flipped(Decoupled(new ExecuteIO))
  val logicalTagComplete = Output(Tag())
  val inputPresent = Output(Bool())
}

class TopLevelPorts(swPorts: SwIO, mainReactorPorts: ReactorIO) extends Module {
  val io = IO(new TopLevelPortsIO(swPorts, mainReactorPorts))

  var outputPorts: ArrayBuffer[TopLevelOutput] = ArrayBuffer()
  var inputPorts: ArrayBuffer[TopLevelInput] = ArrayBuffer()

  require(swPorts.getElements.size == mainReactorPorts.getElements.size, s"swPorts=${swPorts.getElements}, mainReactorPorts=${mainReactorPorts.getElements}")

  // At this point we dont know what kind of ports we have. So we do pattern
  // matching and connect them accordingly
  for ((sw, main) <- io.sw.getElements zip io.mainReactor.getElements) {
    (sw, main) match {
      case (s: SwSingleToken[Data],m: Vec[EventSingleValueReadMaster[Data]])  => {
        println(s"Got SingleToken input")
        // 1. We have an SingleToken Input port. Create the SwPort module and a connection object
        val swPort = Module(new TopLevelInputSingleToken(s.data.cloneType))
        val conn = new SingleValueConnectionFactory(s.data.cloneType)
        conn << swPort.io.main
        conn >> m
        conn.construct()
        swPort.io.swData := s.data // Only connect the data through here. The present signal is handled below
        inputPorts += swPort
      }
      case (s: SwSingleToken[Data], m: EventSingleValueWriteMaster[Data]) => {
        println(s"Got SingleToken Output")
        // 1. We have an SingleToken output port. Connect it and gather all output ports in an array
        // for dealing with the LTC and backpressure.
        val swPort = Module(new TopLevelOutputSingleToken(s.data.cloneType))
        s := swPort.io.sw
        swPort.io.main <> m
        swPort.io.ready := true.B // FIXME: This applies NO backpressure and we can have tokens in the regfile overwritten
        outputPorts += swPort
      }
      case (_,_) => require(false)
    }
  }
  // Now that we have gone through all the top-level ports. We do some final control logic
  // Handle the firing and backpressure on the input port
  io.execute.ready := inputPorts.map(_.io.execute.ready).reduce(_ || _)
  inputPorts.foreach(f => {
    f.io.execute.valid := io.execute.fire
    f.io.execute.bits := io.execute.bits
  })

  // Handle the Output ports and the setting of the LTC
  val ltcHandler = Module(new LtcHandler(outputPorts.size)).io
  ltcHandler.execute.valid := io.execute.fire
  ltcHandler.execute.bits := io.execute.bits
  ltcHandler.outputFires zip outputPorts foreach{f => f._1 := f._2.io.fire}
  io.logicalTagComplete := ltcHandler.logicalTagComplete

  outputPorts.foreach(_.io.consume := io.swCmd === SwCommandType.read)

  // Handle tokenization of the input ports
  val swInputPresent = Module(new SwInputPresent(inputPorts.size))
  swInputPresent.io.write := io.swCmd === SwCommandType.write
  swInputPresent.io.presentIn zip io.sw.getInputs foreach (f => f._1 := f._2.present)
  inputPorts zip swInputPresent.io.presentOut foreach (f => f._1.io.swPresent <> f._2)

  io.inputPresent := swInputPresent.io.presentOut.map(f => f.valid && f.bits).reduceOption(_ || _).getOrElse(false.B)
}

abstract class TopLevelInputIO extends Bundle {
  val execute = Flipped(Decoupled(new ExecuteIO))
  val swPresent = Flipped(Decoupled(Bool()))
}

abstract class TopLevelInput extends Module {
  val io: TopLevelInputIO
}

class TopLevelInputSingleTokenIO[T <: Data](genData: T) extends TopLevelInputIO {
  val swData = Input(genData)
  val main = new EventSingleValueWriteMaster(genData)

  def driveDefaults() = {
    main.driveDefaults()
    execute.ready := false.B
    swPresent.ready := false.B
  }
}

class TopLevelInputSingleToken[T <: Data](genData: T) extends TopLevelInput {
  val io = IO(new TopLevelInputSingleTokenIO(genData))
  io.driveDefaults()
  io.execute.ready := io.main.ready

  when(io.execute.fire) {
    io.main.fire := true.B
    io.swPresent.ready := true.B
    assert(io.swPresent.fire)
    when (EventMode.hasExternalEvent(io.execute.bits.eventMode.asUInt)) {
      io.main.req.valid := true.B
      io.main.req.present := io.swPresent.bits
      io.main.req.token.data := io.swData
      io.main.req.token.tag := io.execute.bits.tag
    }.otherwise {
      io.main.writeAbsent()
    }
  }
}

/**
 *  IO for each top-level Output port
 */
abstract class TopLevelOutputIO extends Bundle {
  val ready = Input(Bool()) // Backpressure signal. Unless high, we will not accept tokens from the main reactor
  val fire = Output(Bool()) // Indicates that tokens from the main reactor have been consumed
  val consume = Input(Bool())
}

abstract class TopLevelOutput extends Module {
  val io: TopLevelOutputIO
}

class TopLevelOutputSingleTokenIO[T <: Data](genData: T) extends TopLevelOutputIO {
  val sw = Output(new SwSingleToken(genData))
  val main = new EventSingleValueWriteSlave(genData)

  def driveDefaults() = {
    main.driveDefaults()
    fire := false.B
  }
}

class TopLevelOutputSingleToken[T <: Data](genData: T) extends TopLevelOutput {
  val io = IO(new TopLevelOutputSingleTokenIO(genData))
  io.driveDefaults()

  val regData = RegInit(0.U.asTypeOf(genData))
  val regPresent = RegInit(false.B)
  io.sw.data := regData
  io.sw.present := regPresent

  io.main.ready := io.ready

  when (io.main.req.valid) {
    regData := io.main.req.token.data
    regPresent := io.main.req.present
    assert(io.ready)
  }

  when(io.consume) {
    regPresent := false.B
    regData := 0.U.asTypeOf(genData)
  }

  when(io.main.fire) {
    io.fire := true.B
  }
}
