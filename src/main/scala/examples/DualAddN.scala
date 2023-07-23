package reactor.examples

import chisel3._
import reactor._
import reactor.globals._

class DualAddN extends Reactor {
  class Reactor1IO extends ReactorIO {
    val in = Vec(1, new EventReadMaster(defData, defToken))
    val out = new EventWriteMaster(defData, defToken)

    def plugUnusedPorts(): Unit = in.foreach(_.driveDefaults())
  }

  val io = IO(new Reactor1IO)
  val r1 = Module(new ReactionAddN(1, ReactionConfig(nPrecedenceOut = 1)))
  val r2 = Module(new ReactionAddN(2, ReactionConfig(nPrecedenceIn = 1)))

  val in = Module(new InputPort(InputPortConfig(defData, defToken, 2)))
  val out = Module(new OutputPort(OutputPortConfig(defData, defToken, 2)))

  in << io.in
  in >> r1.io.in
  in >> r2.io.in

  out << r1.io.out
  out << r2.io.out
  out >> io.out

  r1 > r2


  reactions += r1
  reactions += r2
  inPorts += (in)
  outPorts += (out)

  val triggerIO = connectTimersAndCreateIO()

  reactorMain()
}

