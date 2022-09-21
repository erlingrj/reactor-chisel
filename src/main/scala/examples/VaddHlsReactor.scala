package reactor.examples

import chisel3._
import fpgatidbits.PlatformWrapper._
import reactor._

class vadd_hls_reactor(c: ReactionConfig) extends BlackBoxVitisHls(c) {
  val io = IO(new Bundle {
    val ap = new VitisHlsControlIO()
    val in1 = new VitisHlsInputPort(c.triggers(0))
    val in2 = new VitisHlsInputPort(c.triggers(1))
    val out1 = new VitisHlsOutputPort(c.antiDependencies(0))
  })
  addResource("/HLS/VaddHlsDualPort/vadd_hls_reactor.v")
  addResource("/HLS/VaddHlsDualPort/vadd_hls_reactor_flow_control_loop_pipe.v")
}

class VaddHlsReaction(c: ReactionConfig) extends ReactionHls(c) {

  val blackBoxHls = Module(new vadd_hls_reactor(c))
  connectControlSignals(blackBoxHls.io.ap)
  connectInPort(blackBoxHls.io.in1, io.triggers(0))
  connectInPort(blackBoxHls.io.in2, io.triggers(1))
  connectOutPort(blackBoxHls.io.out1, io.antiDependencies(0))

  makeControlAssertions(blackBoxHls.io.ap)

  def reactionBody: Unit = {
    val done = WireInit(false.B)
    done := blackBoxHls.io.ap.done
    reactionDone := done
  }
  reactionMain
}
class VaddHlsReactor(p: PlatformWrapperParams, vLen: Int = 10) extends ReactorBase(p) {

  // Schduler
  val schedulerConfig = SchedulerConfig(
    Seq(Seq(0))
  )
  val scheduler = Module(new Scheduler(schedulerConfig))
  scheduler.ioSchedulerCtrl.tieOffExt

  // Port
  // Top->r1_in
  val in1_gen = UInt(8.W)
  val in1_cfg = PortConfig(nElems = vLen,nReaders = 1,gen=in1_gen, useBram = false)
  val in1 = Module(new Port(in1_cfg))

  val in2_gen = UInt(8.W)
  val in2_cfg = PortConfig(nElems = vLen,nReaders = 1,gen=in2_gen, useBram = false)
  val in2 = Module(new Port(in2_cfg))

  // r1_out1->r2_in2
  val out_gen = UInt(8.W)
  val out_cfg = PortConfig(nElems = vLen,nReaders = 1, gen=out_gen, useBram = false)
  val out = Module(new Port(out_cfg))


  // Reactions
  val r1_cfg = ReactionConfig(
    triggers = Array(in1_cfg.getPortIOConfig, in2_cfg.getPortIOConfig),
    dependencies = Array(),
    antiDependencies = Array(out_cfg.getPortIOConfig),
    states = Array()
  )
  val r1 = Module(new VaddHlsReaction(r1_cfg))


  // Connections
  // Ports
  in1.io.outs(0) <> r1.io.triggers(0)
  in2.io.outs(0) <> r1.io.triggers(1)
  out.io.in <> r1.io.antiDependencies(0)

  override val inPorts = Array(in1, in2)
  override val outPorts = Array(out)
  override val states = Array()

  // Scheduler
  scheduler.ioReactionCtrl(0) <> r1.ioCtrl

  // DMA
  val dmaConfig = ReactorDMAConfig(
    inPorts = Array(in1_cfg.getPortIOConfig, in2_cfg.getPortIOConfig), outPorts=Array(out_cfg.getPortIOConfig),mrp = p.toMemReqParams()
  )

  val dma = Module(new ReactorDMA(dmaConfig))
  dma.io.tieOffExt

  // Connect DMA to Top-level ports
  dma.io.portRead(0) <> out.io.outs(0)
  dma.io.portWrite(0) <> in1.io.in
  dma.io.portWrite(1) <> in2.io.in

  // Connect DMA to Top-level memory port
  dma.io.memPort <> ioMem.memPort(0)

  connectScheduler2Ports
  reactorMain
}