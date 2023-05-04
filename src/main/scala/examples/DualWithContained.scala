package reactor.examples

import chisel3._
import reactor._
import reactor.globals._

class DualWithContained extends Reactor {
  val r1 = Module(new ReactionAddN(3, ReactionConfig(nPrecedenceOut = 1)))
  val r2 = Module(new ReactionAddN(4, ReactionConfig(nPrecedenceIn = 1)))

  val in = Module(new InputPort(InputPortConfig(defData, defToken, 2)))
  val out = Module(new OutputPort(OutputPortConfig(defData, defToken, 2)))

  in.connectDownstream(r1.io.in)
  in.connectDownstream(r2.io.in)

  out.connectUpstream(r1.io.out)
  out.connectUpstream(r2.io.out)

  r1.precedes(r2)

  val contained = Module(new DualAddN)

  // Handle pass-through input connections
  val in1PT = new InputPortPassthroughBuilder(defData, defToken)
  in1PT.declareDownstream(contained.io.in)

  // Create top-level IO now that we have all port info
  class Reactor2IO extends ReactorIO {
    val in = Vec(1 + in1PT.width, new EventReadMaster(defData, defToken))
    val out1 = new EventWriteMaster(defData, defToken)
    val out2 = new EventWriteMaster(defData, defToken)
  }
  val io = IO(new Reactor2IO)

  // Connect pass through ports
  in1PT.declareInput(io.in.drop(1))

  // Connect regular ports
  in.connectUpstream(io.in(0))
  out.connectDownstream(io.out1)
  contained.io.out <> io.out2 // FIXME: Create a API for this also?

  // Construct the pass-through connection
  val ptInputPorts = in1PT.construct()

  override val reactions = Seq(r1, r2)
  override val inPorts = Seq(in)
  override val outPorts = Seq(out)
  override val childReactors: Seq[Reactor] = Seq(contained)

  reactorMain
}

