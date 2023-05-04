package reactor.examples


import chisel3._
import reactor._
class ReactionAddN(n: Int, c: ReactionConfig = ReactionConfig(0,0)) extends Reaction(c) {
  class IO extends ReactionIO {
    val in = new EventReadMaster(gen = new SingleToken(UInt(8.W)))
    val out = new EventWriteMaster(gen = new SingleToken(UInt(8.W)))
  }
  val io = IO(new IO)
  override val triggers = Seq(io.in)
  override val antiDependencies = Seq(io.out)

  // Bring the port into scope
  val in = io.in
  val out = io.out

  def reactionBody = {
    val read = in.resp.token.data
    val filtered = read + n.U
    out.req.write(filtered)
    reactionDone := true.B
  }

  reactionMain()
}

// FIXME: This actually requires state
class ReactionDownsample(rate: Int, c: ReactionConfig = ReactionConfig(0,0)) extends Reaction {
  class IO extends ReactionIO {
    val in = new EventReadMaster(gen = new SingleToken(UInt(8.W)))
    val out = new EventWriteMaster(gen = new SingleToken(UInt(8.W)))
  }

  val io = IO(new IO)
  override val triggers = Seq(io.in)
  override val antiDependencies = Seq(io.out)

  // Bring the port into scope
  val in = io.in
  val out = io.out

  def reactionBody = {
    assert(false.B)
  }

  reactionMain()

}

class ReactionSum(latency: Int = 0, c: ReactionConfig = ReactionConfig(0,0)) extends Reaction {
  class IO extends ReactionIO {
    val in1 = new EventReadMaster(gen = new SingleToken(UInt(8.W)))
    val in2 = new EventReadMaster(gen = new SingleToken(UInt(8.W)))
    val out = new EventWriteMaster(gen = new SingleToken(UInt(8.W)))
  }

  val io = IO(new IO)
  override val triggers = Seq(io.in1, io.in2)
  override val antiDependencies = Seq(io.out)

  // Bring the port into scope
  val in1 = io.in1
  val in2 = io.in2
  val out = io.out

  def reactionBody = {
    val sum = WireDefault(0.U(8.W))
    val regCount = 0.U(8.W)
    regCount := regCount + 1.U

    when (in1.resp.present) {
      sum := sum + in1.resp.token.data
    }
    when (in2.resp.present) {
      sum := sum + in2.resp.token.data
    }

    when (regCount === latency.U) {
      out.req.write(sum)
      reactionDone := true.B

    }
  }

  reactionMain()

}
