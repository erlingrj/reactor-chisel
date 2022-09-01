package reactor.lib

import chisel3._
import chisel3.util._

import reactor._
import fpgatidbits.PlatformWrapper._

class VaddReactor(p: PlatformWrapperParams) extends ReactorBase(p) {

  // Schduler
  val schedulerConfig = SchedulerConfig(
    Seq(Seq(0))
  )
  val scheduler = Module(new Scheduler(schedulerConfig))
  scheduler.ioSchedulerCtrl.tieOffExt
  val numIn = 10
  val numOut = 5

  def inByteCount =  numIn * 8
  def outByteCount = numOut * 8


  // Reactions
  val r1 = Module(new VaddStreamReaction(numIn))


  // Ports
  // Top->r1_in
  val p_top_r1_in_gen = UInt(8.W)
  val p_top_r1_in_config = PortConfig(nElems = numIn,nReaders = 1,gen=p_top_r1_in_gen)
  val p_top_r1_in = Module(new RegPort(p_top_r1_in_config))

  // r1_out1->r2_in2
  val p_r1_out_top_gen = UInt(8.W)
  val p_r1_out_top_config = PortConfig(nElems = numOut,nReaders = 1, gen=p_r1_out_top_gen)
  val p_r1_out_top = Module(new RegPort(p_r1_out_top_config))

  // Connections
  // Ports
  p_top_r1_in.io.outs(0) <> r1.in
  p_r1_out_top.io.in <> r1.out


  val ports = Seq(p_top_r1_in, p_r1_out_top)

  // Scheduler
  scheduler.ioReactionCtrl(0) <> r1.ioCtrl

  // DMA
  val dmaConfig = ReactorDMAConfig(
    inPorts = Array(p_top_r1_in_config.getPortIOConfig), outPorts=Array(p_r1_out_top_config.getPortIOConfig),mrp = p.toMemReqParams()
  )

  val dma = Module(new ReactorDMA(dmaConfig))
  dma.io.tieOffExt
  dma.io.portRead(0) <> p_r1_out_top.io.outs(0)
  dma.io.portWrite(0) <> p_top_r1_in.io.in
  dma.io.memPort <> ioMem.memPort(0)

  connectScheduler2Ports
  reactorMain
}