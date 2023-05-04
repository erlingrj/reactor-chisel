package reactor.examples

import chisel3._
import reactor._
import reactor.globals._

class DualDataflow extends Reactor {
  val c1 = Module(new DualWithContained)
  val c2 = Module(new DualAddN)
  val c3 = Module(new DualAddN)

  // Connect c1->c2
  val c1_c2_func = (c: ConnectionConfig[UInt, SingleToken[UInt]]) => new SingleValueConnection(c)
  val c1_c2_builder = new ConnectionBuilder(c1_c2_func, defData, defToken)
  c1_c2_builder.addUpstream(c1.io.out1)
  c1_c2_builder.addDownstream(c2.io.in)
  val c1_c2_conn = c1_c2_builder.construct()

  // Connect c1->c3
  val c1_c3_func = (c: ConnectionConfig[UInt, SingleToken[UInt]]) => new SingleValueConnection(c)
  val c1_c3_builder = new ConnectionBuilder(c1_c3_func, defData, defToken)
  c1_c3_builder.addUpstream(c1.io.out2)
  c1_c3_builder.addDownstream(c3.io.in)
  val c1_c3_conn = c1_c3_builder.construct()

  // Handle pass-through input connections
  val in1PT = new InputPortPassthroughBuilder(defData, defToken)
  in1PT.declareDownstream(c1.io.in)

  // Create top-level IO now that we have all port info
  class Reactor2IO extends ReactorIO {
    val in = Vec(0 + in1PT.width, new EventReadMaster(defData, defToken))
    val out1 = new EventWriteMaster(defData, defToken)
    val out2 = new EventWriteMaster(defData, defToken)
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

