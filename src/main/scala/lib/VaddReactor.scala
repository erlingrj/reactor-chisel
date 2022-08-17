package reactor.lib

import chisel3._
import chisel3.util._

import reactor._
import fpgatidbits.PlatformWrapper._

class VaddReactor(p: PlatformWrapperParams)(implicit rp: ReactorGlobalParams) extends ReactorBase(p) {

  // Schduler
  val schedulerConfig = SchedulerConfig(
    Seq(Seq(0,1), Seq(2))
  )
  val scheduler = Module(new Scheduler(schedulerConfig))


  val addLen=10

  // Reactions
  val r0 = Module(new VaddReaction(addLen))
  val r1 = Module(new VaddReaction(addLen))
  val r2 = Module(new VaddReaction(addLen))

  // Ports

  // Top->r0_in1
  val p_top_r0_in1_gen = UInt(8.W)
  val p_top_r0_in1_config = PortConfig(addLen,1)
  val p_top_r0_in1 = Module(new RegPort(p_top_r0_in1_config, p_top_r0_in1_gen))

  // Top->r0_in2
  val p_top_r0_in2_gen = UInt(8.W)
  val p_top_r0_in2_config = PortConfig(addLen,1)
  val p_top_r0_in2 = Module(new RegPort(p_top_r0_in2_config, p_top_r0_in2_gen))

  // Top->r0_in1
  val p_top_r1_in1_gen = UInt(8.W)
  val p_top_r1_in1_config = PortConfig(addLen,1)
  val p_top_r1_in1 = Module(new RegPort(p_top_r1_in1_config, p_top_r1_in1_gen))

  // Top->r0_in2
  val p_top_r1_in2_gen = UInt(8.W)
  val p_top_r1_in2_config = PortConfig(addLen,1)
  val p_top_r1_in2 = Module(new RegPort(p_top_r1_in2_config, p_top_r1_in2_gen))

  // r0_out1->r2_in1
  val p_r0_out1_r2_in1_gen = UInt(8.W)
  val p_r0_out1_r2_in1_config = PortConfig(addLen,1)
  val p_r0_out1_r2_in1 = Module(new RegPort(p_r0_out1_r2_in1_config, p_r0_out1_r2_in1_gen))

  // r1_out1->r2_in2
  val p_r1_out1_r2_in2_gen = UInt(8.W)
  val p_r1_out1_r2_in2_config = PortConfig(addLen,1)
  val p_r1_out1_r2_in2 = Module(new RegPort(p_r1_out1_r2_in2_config, p_r1_out1_r2_in2_gen))

  // Connections
  // Ports
  p_top_r0_in1.io.outs(0) <> r0.in1
  p_top_r0_in2.io.outs(0) <> r0.in2

  p_top_r1_in1.io.outs(0) <> r1.in1
  p_top_r1_in2.io.outs(0) <> r1.in2

  r0.out1 <> p_r0_out1_r2_in1.io.in
  r1.out1 <> p_r1_out1_r2_in2.io.in

  p_r0_out1_r2_in1.io.outs(0) <> r2.in1
  p_r1_out1_r2_in2.io.outs(0) <> r2.in2

  // Scheduler
  scheduler.ioReactionCtrl(0) <> r0.ioCtrl
  scheduler.ioReactionCtrl(1) <> r1.ioCtrl
  scheduler.ioReactionCtrl(2) <> r2.ioCtrl

  // Memory Reader

  // Memory Writer

  // Top-level state machine

}
