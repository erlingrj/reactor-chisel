package reactor.examples

import chisel3._
import chisel3.util.HasBlackBoxResource
import reactor._

class vadd_hls_reactor(c: ReactionConfig) extends BlackBoxVitisHls(c) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val ap = new VitisHlsControlIO()
    val in1 = new VitisHlsInputPort(c.triggers(0))
    val in2 = new VitisHlsInputPort(c.triggers(1))
    val out1 = new VitisHlsOutputPort(c.antiDependencies(0))
  })
  addResource("/HLS/VaddHlsDualPort/vadd_hls_reactor.v")
 // addResource("/HLS/VaddHlsDualPort/vadd_hls_reactor_flow_control_loop_pipe.v")
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

