package reactor.lib

import chisel3._
import chisel3.util._

import reactor._
import fpgatidbits.PlatformWrapper._

class VaddReactor(p: PlatformWrapperParams)(implicit rp: ReactorGlobalParams) extends ReactorBase(p) {

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
  val p_top_r1_in_config = PortConfig(numIn,1)
  val p_top_r1_in = Module(new RegPort(p_top_r1_in_config, p_top_r1_in_gen))

  // r1_out1->r2_in2
  val p_r1_out_top_gen = UInt(8.W)
  val p_r1_out_top_config = PortConfig(numOut,1)
  val p_r1_out_top = Module(new RegPort(p_r1_out_top_config, p_r1_out_top_gen))

  // Connections
  // Ports
  p_top_r1_in.io.outs(0) <> r1.in
  p_r1_out_top.io.in <> r1.out


  val ports = Seq(p_top_r1_in, p_r1_out_top)

  // Scheduler
  scheduler.ioReactionCtrl(0) <> r1.ioCtrl

  // DMA
  val dmaConfig = ReactorDMAConfig(
    nElemsIn = numIn, nElemsOut = numOut, mrp = p.toMemReqParams()
  )

  val dma = Module(new ReactorDMA(dmaConfig, p_top_r1_in_gen, p_r1_out_top_gen))
  dma.io.tieOffExt
  dma.io.portRead <> p_r1_out_top.io.outs(0)
  dma.io.portWrite <> p_top_r1_in.io.in
  dma.io.memPort <> ioMem.memPort(0)

  connectScheduler2Ports
  reactorMain
}