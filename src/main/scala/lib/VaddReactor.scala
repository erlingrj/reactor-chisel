package reactor.lib

import chisel3._
import chisel3.util._

import reactor._
import fpgatidbits.PlatformWrapper._

class VaddReactor(p: PlatformWrapperParams, vLen: Int = 10) extends ReactorBase(p) {

  // Schduler
  val schedulerConfig = SchedulerConfig(
    Seq(Seq(0))
  )
  val scheduler = Module(new Scheduler(schedulerConfig))
  scheduler.ioSchedulerCtrl.tieOffExt

  // Port
  // Top->r1_in
  val in1_gen = UInt(8.W)
  val in1_cfg = PortConfig(nElems = vLen,nReaders = 1,gen=in1_gen)
  val in1 = Module(new RegPort(in1_cfg))

  val in2_gen = UInt(8.W)
  val in2_cfg = PortConfig(nElems = vLen,nReaders = 1,gen=in2_gen)
  val in2 = Module(new RegPort(in2_cfg))

  // r1_out1->r2_in2
  val out_gen = UInt(8.W)
  val out_cfg = PortConfig(nElems = vLen,nReaders = 1, gen=out_gen)
  val out = Module(new RegPort(out_cfg))


  // Reactions
  val r1_cfg = ReactionConfig(
    triggers = Array(in1_cfg.getPortIOConfig, in2_cfg.getPortIOConfig),
    dependencies = Array(),
    antiDependencies = Array(out_cfg.getPortIOConfig)
  )
  val r1 = Module(new VaddStreamReaction(r1_cfg))


  // Connections
  // Ports
  in1.io.outs(0) <> r1.io.triggers(0)
  in2.io.outs(0) <> r1.io.triggers(1)
  out.io.in <> r1.io.antiDependencies(0)

  override val inPorts = Seq(in1, in2)
  override val outPorts = Seq(out)

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