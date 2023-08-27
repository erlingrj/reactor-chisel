package reactor
import chisel3._

class StandaloneTopReactorIO extends Bundle {
  val terminate = Output(Bool())
}

class StandaloneTopReactor(mainReactorGenFunc: () => Reactor)(implicit globalCfg: GlobalReactorConfig) extends Module {
  val mainReactor = Module(mainReactorGenFunc())
  val io = IO(new StandaloneTopReactorIO)
  val externalIO = IO(mainReactor.externalIO.cloneType)
  val physicalIO = IO(new ReactorPhysicalFlippedIO(mainReactor.physicalIO.cloneType))
  externalIO <> mainReactor.externalIO

  val trigGen = Module(new TriggerGenerator(true, globalCfg.timeout, mainReactor))

  for (((main, ext), idx) <- (mainReactor.physicalIO.getAllPorts zip physicalIO.getAllPorts).zipWithIndex){
    val (connFactory, mainIO, extIO) = (main, ext) match {
      case (m: EventPureReadMaster, e: EventPureWriteSlave) =>
        val c = new PureConnectionFactory
        c << e
        c >> m
        (c,m,e)
      case (m: EventSingleValueReadMaster[Data], e: EventSingleValueWriteSlave[Data]) =>
        val c = new SingleValueConnectionFactory(e.genData)
        c << e
        c >> m
        (c,m,e)
      case (m: EventArrayReadMaster[Data], e: EventArrayWriteSlave[Data]) =>
        val c = new ArrayConnectionFactory(e.genData, e.genToken)
        c << e
        c >> m
        (c,m,e)
    }
    val conn = connFactory.construct()
    val trigGenTrigger = trigGen.io.phyTriggers(idx)
    val trigGenSched = trigGen.io.phySchedules(idx)
    // Let the TriggerGenerator control when this connection fires and the tag it will be associated with
    conn.head.io.write.fire := trigGenTrigger.fire
    conn.head.io.write.req.token.asInstanceOf[Token[UInt]].tag := trigGenTrigger.req.token.tag // FIXME: Hacky way of overriding the tag signal so that the TriggerGenerator decides the tag of any Physical Action event

    trigGenTrigger.ready := conn.head.io.write.ready

    // Connect the fire signal from the top-level IO port to the TriggerGenerator
    trigGenSched.fire := extIO.fire
    trigGenSched.req.driveDefaults() // We only need the fire signal.
  }


  // Connect the triggerGenerator to the mainReactor
  for (i <- mainReactor.triggerIO.allTimerTriggers.indices) {
    mainReactor.triggerIO.allTimerTriggers(i) <> trigGen.io.timerTriggers(i)
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
