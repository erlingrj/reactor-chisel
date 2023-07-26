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

  val triggerGenerator = Module(new MainTriggerGenerator(mainReactor))

  // Connect the triggerGenerator to the mainReactor
  val mainReactorTriggerIO = mainReactor.triggerIO.localTriggers ++ mainReactor.triggerIO.containedTriggers
  for ((triggerGen, reactorTrigger) <- triggerGenerator.io.timers zip mainReactorTriggerIO) {
    reactorTrigger <> triggerGen.trigger
  }

  triggerGenerator.io.tagAdvanceGrant := Tag.FOREVER
  triggerGenerator.io.mainReactorIdle := mainReactor.io.idle
  // Plug any top-level
  mainReactor.io.driveDefaultsFlipped()

  io.terminate := triggerGenerator.io.terminate
}
