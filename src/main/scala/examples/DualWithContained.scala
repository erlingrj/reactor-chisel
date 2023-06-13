package reactor.examples

import chisel3._
import reactor._
import reactor.globals._

class DualWithContained extends Reactor {
  val r1 = Module(new ReactionAddN(3, ReactionConfig(nPrecedenceOut = 1)))
  val r2 = Module(new ReactionAddN(4, ReactionConfig(nPrecedenceIn = 1)))

  val in = Module(new InputPort(InputPortConfig(defData, defToken, 2)))
  val out = Module(new OutputPort(OutputPortConfig(defData, defToken, 2)))

  in >> r1.io.in
  in >> r2.io.in

  out << r1.io.out
  out << r2.io.out

  // Precedence relationships
  r1 > r2

  val contained = Module(new DualAddN)

  // Handle pass-through input connections
  val in1PT = new InputPortInwardConnectionFactory(defData, defToken)
  in1PT.declareDownstream(contained.io.in)

  // Create top-level IO now that we have all port info
  class Reactor2IO extends ReactorIO {
    val in = Vec(1 + in1PT.width, new EventReadMaster(defData, defToken))
    val out1 = new EventWriteMaster(defData, defToken)
    val out2 = new EventWriteMaster(defData, defToken)
    def plugUnusedPorts(): Unit = in.foreach(_.driveDefaults())
  }
  val io = IO(new Reactor2IO)

  // Connect pass through ports
  in1PT.declareInput(io.in.drop(1))

  // Connect regular ports
  in << io.in
  out >> io.out1

  contained.io.out <> io.out2 // FIXME: Create a API for this also?

  // Construct the pass-through connection
  val ptInputPorts = in1PT.construct()

  reactions = Seq(r1, r2)
  inPorts = Seq(in)
  outPorts = Seq(out)
  childReactors = Seq(contained)

  val timerIO = connectTimersAndCreateIO()

  reactorMain()
}

