package reactor.examples

import chisel3._
import reactor._

class DualDataflow extends Reactor {
  val gen = new SingleToken(UInt(8.W))

  val c1 = Module(new DualWithContained)
  val c2 = Module(new DualAddN)
  val c3 = Module(new DualAddN)

  // Connect c1->c2
  val c1_c2_gen = gen
  val c1_c2_func = (c: ConnectionConfig[SingleToken[UInt]]) => new SingleValueConnection(c)
  val c1_c2_builder = new ConnectionBuilder(c1_c2_func, c1_c2_gen)
  c1_c2_builder.addUpstream(c1.io.out1)
  c1_c2_builder.addDownstream(c2.io.in)
  val c1_c2_conn = c1_c2_builder.construct()

  // Connect c1->c3
  val c1_c3_gen = gen
  val c1_c3_func = (c: ConnectionConfig[SingleToken[UInt]]) => new SingleValueConnection(c)
  val c1_c3_builder = new ConnectionBuilder(c1_c3_func, c1_c3_gen)
  c1_c3_builder.addUpstream(c1.io.out2)
  c1_c3_builder.addDownstream(c3.io.in)
  val c1_c3_conn = c1_c3_builder.construct()

  // Handle pass-through input connections
  val in1PT = new InputPortPassthroughBuilder(gen)
  in1PT.declareDownstream(c1.io.in)

  // Create top-level IO now that we have all port info
  class Reactor2IO extends ReactorIO {
    val in = Vec(0 + in1PT.width, new EventReadMaster(gen))
    val out1 = new EventWriteMaster(gen)
    val out2 = new EventWriteMaster(gen)
  }

  val io = IO(new Reactor2IO)

  // Connect pass through ports
  in1PT.declareInput(io.in.drop(0))

  // Handle pass-through output ports
  c2.io.out <> io.out1
  c3.io.out <> io.out2

  // Construct the pass-through connection
  in1PT.construct()

  reactorMain
}

