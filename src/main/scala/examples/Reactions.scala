package reactor.examples


import chisel3._
import reactor._
import reactor.globals._
import reactor.ReactionApi._

class ReactionAddN(n: Int, c: ReactionConfig = ReactionConfig(0,0)) extends Reaction(c) {
  val gen1 = UInt(8.W)
  val gen2 = new SingleToken(gen1)
  class myIO extends ReactionIO {
    val in = new EventReadMaster(gen1, gen2)
    val out = new EventWriteMaster(gen1, gen2)
  }
  val io = IO(new myIO())

  class StateIO extends ReactionStateIO {}
  val stateIO = IO(new StateIO)

  override val triggers = Seq(io.in)
  override val antiDependencies = Seq(io.out)

  // Bring the port into scope
  val in = io.in
  val out = io.out

  def reactionBody() ={
    val read = lf_get(in)
    val filtered = read + n.U
    lf_set(out, filtered)
    true.B
  }

  reactionMain()
}

// FIXME: This actually requires state
class ReactionDownsample(rate: Int, c: ReactionConfig = ReactionConfig(0,0)) extends Reaction {
  val gen1 = UInt(8.W)
  val gen2 = new SingleToken(gen1)

  class IO extends ReactionIO {
    val in = new EventReadMaster(gen1, gen2)
    val out = new EventWriteMaster(gen1, gen2)
  }

  val io = IO(new IO)

  class StateIO extends ReactionStateIO {}
  val stateIO = IO(new StateIO)

  override val triggers = Seq(io.in)
  override val antiDependencies = Seq(io.out)

  // Bring the port into scope
  val in = io.in
  val out = io.out

  def reactionBody() = {
    assert(false.B)
    true.B
  }

  reactionMain()

}

class ReactionSum(latency: Int = 0, c: ReactionConfig = ReactionConfig(0,0)) extends Reaction {
  val gen1 = UInt(8.W)
  val gen2 = new SingleToken(gen1)

  class IO extends ReactionIO {
    val in1 = new EventReadMaster(gen1,gen2)
    val in2 = new EventReadMaster(gen1, gen2)
    val out = new EventWriteMaster(gen1, gen2)
  }

  val io = IO(new IO)

  class StateIO extends ReactionStateIO {}
  val stateIO = IO(new StateIO)

  override val triggers = Seq(io.in1, io.in2)
  override val antiDependencies = Seq(io.out)

  // Bring the port into scope
  val in1 = io.in1
  val in2 = io.in2
  val out = io.out

  def reactionBody() = {
    val sum = WireDefault(0.U(8.W))
    val regCount = 0.U(8.W)
    regCount := regCount + 1.U

    when (lf_present(in1)) {
      sum := sum + lf_get(in1)
    }
    when (lf_present(in2)) {
      sum := sum + lf_get(in2)
    }

    when (regCount === latency.U) {
      lf_set(out, sum)
      true.B
    }
    false.B
  }
  reactionMain()
}
class ReactionAddMultipleWidths(c: ReactionConfig = ReactionConfig(0,0)) extends Reaction(c) {
  val in1_genData = UInt(8.W)
  val in1_genToken = new SingleToken(in1_genData)

  val in2_genData = UInt(16.W)
  val in2_genToken = new SingleToken(in2_genData)

  val out_genData = UInt(16.W)
  val out_genToken = new SingleToken(out_genData)
  class IO extends ReactionIO {
    val in1 = new EventReadMaster(in1_genData, in1_genToken)
    val in2 = new EventReadMaster(in2_genData, in2_genToken)
    val out = new EventWriteMaster(out_genData, out_genToken)
  }
  val io = IO(new IO)

  class StateIO extends ReactionStateIO {}
  val stateIO = IO(new StateIO)

  override val triggers = Seq(io.in1, io.in2)
  override val antiDependencies = Seq(io.out)

  // Bring the port into scope
  val in1 = io.in1
  val in2 = io.in2
  val out = io.out

  def reactionBody() ={
    val sum = lf_get(in1) + lf_get(in2)
    lf_set(out, sum)
    true.B
  }

  reactionMain()
}
class ReactionPurePrint(c: ReactionConfig = ReactionConfig(0,0)) extends Reaction(c) {
  class IO extends ReactionIO {
    val t = new EventReadMaster(pureData, new PureToken)
  }
  val io = IO(new IO)

  class StateIO extends ReactionStateIO {}
  val stateIO = IO(new StateIO)

  override val triggers = Seq(io.t)

  // Bring the port into scope
  val t = io.t

  def reactionBody() ={
    printf("Reaction triggered @ logical tag: %d physical tag: %d\n", lf_time_logical(), lf_time_physical())
    true.B
  }

  reactionMain()
}


class ReactionCountPrint(c: ReactionConfig = ReactionConfig(0,0)) extends Reaction(c) {
  class IO extends ReactionIO {
    val t = new EventReadMaster(pureData, new PureToken)
  }

  val io = IO(new IO)

  class StateIO extends ReactionStateIO {
    val cnt = new StateReadWriteMaster(defData, defToken)
  }

  val stateIO = IO(new StateIO)
  // FIXME: Combine
  stateIO.cnt.read.driveDefaults()
  stateIO.cnt.write.driveDefaults()

  override val triggers = Seq(io.t)

  // Bring the port into scope
  val t = io.t

  // Bring state variables into scope
  val cnt = stateIO.cnt

  def reactionBody() = {
    printf("Count=%d Reaction triggered @ logical tag: %d physical tag: %d\n", lf_read(cnt), lf_time_logical(), lf_time_physical())
    lf_write(cnt, lf_read(cnt) + 1.U)
    true.B
  }

  reactionMain()
}


class ReactionCount(c: ReactionConfig = ReactionConfig(0,0)) extends Reaction(c) {
  class IO extends ReactionIO {
    val t = new EventReadMaster(pureData, new PureToken)
  }

  val io = IO(new IO)

  class StateIO extends ReactionStateIO {
    val cnt = new StateReadWriteMaster(defData, defToken)
  }

  val stateIO = IO(new StateIO)
  // FIXME: Combine
  stateIO.cnt.read.driveDefaults()
  stateIO.cnt.write.driveDefaults()

  override val triggers = Seq(io.t)

  // Bring the port into scope
  val t = io.t

  // Bring state variables into scop
  val cnt = stateIO.cnt

  def reactionBody() = {
    lf_write(cnt, lf_read(cnt) + 1.U)
    true.B
  }
  reactionMain()
}

class ReactionPrintCount(c: ReactionConfig = ReactionConfig(0,0)) extends Reaction(c) {
  class IO extends ReactionIO {
    val t = new EventReadMaster(pureData, new PureToken)
  }

  val io = IO(new IO)

  class StateIO extends ReactionStateIO {
    val cnt = new StateReadWriteMaster(defData, defToken)
  }

  val stateIO = IO(new StateIO)
  // FIXME: Combine
  stateIO.cnt.read.driveDefaults()
  stateIO.cnt.write.driveDefaults()

  override val triggers = Seq(io.t)

  // Bring the port into scope
  val t = io.t

  // Bring state variables into scop
  val cnt = stateIO.cnt

  def reactionBody() = {
    printf("Count=%d\n", lf_read(cnt))
    true.B
  }
  reactionMain()
}

