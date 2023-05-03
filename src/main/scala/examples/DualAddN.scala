package reactor.examples

import chisel3._
import reactor._

class DualAddN extends Reactor {
  val gen = new SingleToken(UInt(8.W))
  class Reactor1IO extends ReactorIO {
    val in = Vec(1, new EventReadMaster(gen))
    val out = new EventWriteMaster(gen)
  }
  val io = IO(new Reactor1IO)
  val r1 = Module(new ReactionAddN(1, ReactionConfig(nPrecedenceOut = 1)))
  val r2 = Module(new ReactionAddN(2, ReactionConfig(nPrecedenceIn = 1)))

  val in = Module(new InputPort(InputPortConfig(gen, 2)))
  val out = Module(new OutputPort(OutputPortConfig(gen, 2)))

  in.connectDownstream(r1.io.in)
  in.connectDownstream(r2.io.in)
  in.connectUpstream(io.in(0))

  out.connectUpstream(r1.io.out)
  out.connectUpstream(r2.io.out)
  out.connectDownstream(io.out)

  r1.precedes(r2)

  override val reactions = Seq(r1, r2)
  override val inPorts = Seq(in)
  override val outPorts = Seq(out)

  reactorMain
}

