package reactor.examples

import chisel3._
import chisel3.util._
import fpgatidbits.PlatformWrapper._
import reactor._
import reactor.lib._

class StatefulReaction(c: ReactionConfig) extends Reaction(c) {

  val in1_cfg = PortIOConfig(nElems = 1, gen=UInt(8.W))
  val in1 = io.triggers(0).asInstanceOf[PortOutIO[UInt]]

  val in2_cfg = PortIOConfig(nElems = 1, gen=UInt(8.W))
  val in2 = io.triggers(1).asInstanceOf[PortOutIO[UInt]]
  val out1 = io.antiDependencies(0).asInstanceOf[PortInIO[UInt]]

  val runningSum = ioState.states(0).asInstanceOf[ReactorStateIO[UInt]]

  def reactionBody: Unit = {

    val done = WireInit(false.B)

    val sRead :: sExec :: sDone :: Nil = Enum(3)
    val regState = RegInit(sRead)
    switch(regState) {
      is (sRead) {
        in1.read(0.U)
        in2.read(0.U)
        runningSum.read(0.U)
        regState := sExec
      }
      is (sExec) {
        val sum = in1.readRsp + in2.readRsp + runningSum.readRsp
        out1.write(0.U, sum)
        runningSum.write(0.U, sum)
        regState := sDone
      }
      is (sDone) {
        done := true.B
      }
    }
    reactionDone := done
  }
  reactionMain
}


class StatefulReactor(p: PlatformWrapperParams) extends ReactorBase(p) {

  // Schduler
  val schedulerConfig = SchedulerConfig(
    Seq(Seq(0))
  )
  val scheduler = Module(new Scheduler(schedulerConfig))
  scheduler.ioSchedulerCtrl.tieOffExt

  // Port
  // Top->r1_in
  val in1_gen = UInt(8.W)
  val in1_cfg = PortConfig(nElems = 1,nReaders = 1,gen=in1_gen, useBram = false)
  val in1 = Module(new Port(in1_cfg))

  val in2_gen = UInt(8.W)
  val in2_cfg = PortConfig(nElems = 1,nReaders = 1,gen=in2_gen, useBram = false)
  val in2 = Module(new Port(in2_cfg))

  // r1_out1->r2_in2
  val out_gen = UInt(8.W)
  val out_cfg = PortConfig(nElems = 1,nReaders = 1, gen=out_gen, useBram = false)
  val out = Module(new Port(out_cfg))

  //s_runningSum
  val runningSum_gen = UInt(8.W)
  val runningSum_cfg = ReactorStateConfig(
    nElems = 1, gen = runningSum_gen, useBram = false
  )
  val runningSum = Module(new ReactorState(runningSum_cfg))

  // Reactions
  val r1_cfg = ReactionConfig(
    triggers = Array(in1_cfg.getPortIOConfig, in2_cfg.getPortIOConfig),
    dependencies = Array(),
    antiDependencies = Array(out_cfg.getPortIOConfig),
    states = Array(runningSum_cfg)
  )
  val r1 = Module(new StatefulReaction(r1_cfg))


  // Connections
  // Ports
  in1.io.outs(0) <> r1.io.triggers(0)
  in2.io.outs(0) <> r1.io.triggers(1)
  out.io.in <> r1.io.antiDependencies(0)

  // States
  runningSum.io <> r1.ioState.states(0)

  override val inPorts = Array(in1, in2)
  override val outPorts = Array(out)
  override val states = Array(runningSum)

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
