package reactor

import Chisel.Decoupled
import chisel3._


class TimerTriggerIO(nTimers: Int) extends Bundle {
  val timers = Vec(nTimers, new TriggerIO())
}

class CoordinationIO extends Bundle {
  val nextEventTag = Output(new Tag)
  val tagAdvanceGrant = Input(new Tag)
  val logicalTagComplete = Output(new Tag)

  def driveDefaults(): Unit = {
    nextEventTag.time := 0.U
    logicalTagComplete := Tag.FOREVER
  }
}

class SwIO(swInputsGen: () => SwInputs, swOuptutsGen: () => SwOutputs) extends Bundle {
  val swInputs = Input(swInputsGen())
  val swOutputs = Output(swOuptutsGen())
  val test = Flipped(Decoupled(Bool()))
}

class MainReactorIO(mainReactorInputsGen: () => Bundle, mainReactorOutputsGen: () => Bundle) extends Bundle {
  val mainReactorInputs = mainReactorInputsGen
  val mainReactorOutputs = mainReactorOutputsGen
}

case class TriggerGeneratorParams (
  mainReactor: Reactor,
  swInputsGen: () => SwInputs,
  swOutputsGen: () => SwOutputs,
  mainReactorInputsGen: () => Bundle,
  mainReactorOutputsGen: () => Bundle,
)

class TriggerGenerator(p: TriggerGeneratorParams) extends Module {
  val timerTriggerIO = IO(new TimerTriggerIO(p.mainReactor.triggerIO.allTriggers.size))
  val coordinationIO = IO(new CoordinationIO())
  val SwIO = IO(new SwIO(p.swInputsGen, p.swOutputsGen))
  val mainReactorIO = IO(new MainReactorIO(p.mainReactorInputsGen, p.mainReactorOutputsGen))


  val scheduleParams = ScheduleParams(
    triggerVecs =
  )

}

class MainTriggerGeneratorIO(nTimers: Int) extends Bundle {
  val timers = Vec(nTimers, new TriggerIO())
  val nextEventTag = Output(new Tag)
  val tagAdvanceGrant = Input(new Tag)
  val mainReactorIdle = Input(Bool())
  val terminate = Output(Bool())

  def driveDefaults(): Unit = {
    if (directionOf(nextEventTag) == ActualDirection.Output) {
      nextEventTag.time := 0.U
      terminate := false.B
    } else {
      tagAdvanceGrant := 0.U
    }
  }
}

// FIXME: This MainTimer doesnt do any time coordination yet. It should have
class MainTriggerGenerator(mainReactor: Reactor)(implicit val globalCfg: GlobalReactorConfig) extends Module {
  val mainReactorTriggers = mainReactor.localTriggers ++ mainReactor.containedTriggers
  val mainReactorTriggerIOs = mainReactor.triggerIO.localTriggers ++ mainReactor.triggerIO.containedTriggers
  val io = IO(new MainTriggerGeneratorIO(mainReactorTriggers.size))
  io.driveDefaults()

  println(s"MainTimer created for program where main has  ${mainReactor.containedTriggers.size} contained + ${mainReactor.containedTriggers.size} top-level timers")

  // Create the HW counters
  // FIXME: This should be optimized. I am reusing the old timer module, we can instead create a
  //  single timer with a single hyperperiod
  val allTimerConfigs = mainReactorTriggers.map(_.cfg)
  val timers = for (t <- mainReactorTriggers) yield Module(new Timer(t.cfg, allTimerConfigs.toSeq.diff(Seq(t.cfg)))(globalCfg))
  // Connect the HW counters to top-level IO through a FIFO to allow some buffering
  for ((timer, timerIO) <- timers zip io.timers) {
    val fifo = Module(new EventWriteQueue(genData = UInt(0.W), genToken = new PureToken, nEntries = 2))
    fifo.io.enq <> timer.io.trigger
    fifo.io.deq <> timerIO.trigger
  }

  // Termination condition. 10 CCs with everything idle
  val terminate = timers.map(_.coordinationIo.idle).reduceOption(_ && _).getOrElse(true.B) && io.mainReactorIdle
  val regTerminateCountDown = RegInit(10.U)

  when (terminate) {
    regTerminateCountDown := regTerminateCountDown - 1.U
  } otherwise {
    regTerminateCountDown := 10.U
  }

  io.terminate := regTerminateCountDown === 0.U
}
