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
  val c1_c2 = new ConnectionFactory(c1_c2_func, defData, defToken)
  c1_c2 << c1.io.out1
  c1_c2 >> c2.io.in
  c1_c2.construct()


  // Connect c1->c3
  val c1_c3_func = (c: ConnectionConfig[UInt, SingleToken[UInt]]) => new SingleValueConnection(c)
  val c1_c3 = new ConnectionFactory(c1_c3_func, defData, defToken)
  c1_c3 << (c1.io.out2)
  c1_c3 >> (c3.io.in)
  c1_c3.construct()

  // Handle pass-through input connections
  val in1PT = new InputPortInwardConnectionFactory(defData, defToken)
  in1PT >> (c1.io.in)

  // Create top-level IO now that we have all port info
  class Reactor2IO extends ReactorIO {
    val in = Vec(0 + in1PT.width, new EventReadMaster(defData, defToken))
    val out1 = new EventWriteMaster(defData, defToken)
    val out2 = new EventWriteMaster(defData, defToken)

    def plugUnusedPorts(): Unit = in.foreach(_.driveDefaults())
  }

  val io = IO(new Reactor2IO)

  // Connect pass through ports
  in1PT << io.in

  // Handle pass-through output ports
  c2.io.out <> io.out1
  c3.io.out <> io.out2

  // Construct the pass-through connection
  in1PT.construct()

  val timerIO = connectTimersAndCreateIO()

  reactorMain()
}

