package reactor
import chisel3._

class StandaloneTopReactorIO extends Bundle {
  val terminate = Output(Bool())
}

class StandaloneTopReactor(mainReactorGenFunc: () => Reactor)(implicit globalCfg: GlobalReactorConfig) extends Module {
  val mainReactor = Module(mainReactorGenFunc())
  val io = IO(new StandaloneTopReactorIO)
  val externalIO = IO(mainReactor.externalIO.cloneType)
  externalIO <> mainReactor.externalIO

  val trigGen = Module(new TriggerGenerator(true, globalCfg.timeout, mainReactor))

  // Connect the triggerGenerator to the mainReactor
  for (i <- mainReactor.triggerIO.allTimerTriggers.indices) {
    mainReactor.triggerIO.allTimerTriggers(i) <> trigGen.io.triggers(i)
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
