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

  val triggerGenerator = Module(new TriggerGenerator(globalCfg.timeout, mainReactor))

  // Connect the triggerGenerator to the mainReactor
  for (i <- mainReactor.triggerIO.allTriggers.indices) {
    mainReactor.triggerIO.allTriggers(i) <> triggerGenerator.io.triggers.timers(i).trigger
  }

  val coordinationIO = triggerGenerator.io.coordination
  triggerGenerator.io.inputPresent := false.B
  coordinationIO.tagAdvanceGrant := Tag.FOREVER
  // Plug any top-level
  mainReactor.io.driveDefaultsFlipped()

  // Terminate when triggerGenerator has fired the shutdown trigger AND all reactors are idle
  io.terminate := triggerGenerator.io.terminate && mainReactor.statusIO.idle
}
