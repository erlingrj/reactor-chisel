package reactor
import chisel3._

class StandaloneTopReactorIO extends Bundle {
  val terminate = Output(Bool())
}

class StandaloneTopReactor(mainReactorGenFunc: () => Reactor)(implicit val globalCfg: GlobalReactorConfig) extends Module {
  val mainReactor = Module(mainReactorGenFunc())
  val io = IO(new StandaloneTopReactorIO)
  val externalIO = IO(mainReactor.externalIO.cloneType)
  val physicalIO = IO(new ReactorPhysicalFlippedIO(mainReactor.physicalIO.cloneType))
  externalIO <> mainReactor.externalIO

  val trigGen = Module(new TriggerGenerator(mainReactor))

  // Connect external physical IO, TriggerGenerator and physical IO on the main Reactor
  PhysicalActionConnector(mainReactor.physicalIO, physicalIO, trigGen.io)

  // Connect the triggerGenerator to the mainReactor
  for (i <- mainReactor.triggerIO.allTimerTriggers.indices) {
    val fifo = Module(new PureTokenQueue(32)).io // FIXME: Make configurable the number of fifo elements.
    trigGen.io.timerTriggers(i) <> fifo.enq
    fifo.deq <> mainReactor.triggerIO.allTimerTriggers(i)
  }

  trigGen.io.inputPresent := false.B
  trigGen.io.execute.ready := true.B
  trigGen.io.shutdownCommand:= 0.U.asTypeOf(new ShutdownCommand)
  trigGen.io.tagAdvanceGrant := Tag.FOREVER
  trigGen.io.coordinationValid := true.B
  // Plug any top-level
  mainReactor.io.driveDefaultsFlipped()

  // Terminate when triggerGenerator has fired the shutdown trigger AND all reactors are idle
  io.terminate := trigGen.io.terminate && mainReactor.statusIO.idle
}
