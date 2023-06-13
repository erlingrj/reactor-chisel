package reactor.examples

import reactor._
import reactor.globals._

import chisel3._
import chisel3.util._

class ReactorCounter extends Reactor {
  class Reactor1IO extends ReactorIO {
    def plugUnusedPorts() = {}
  }

  val io = IO(new Reactor1IO)
  val t = new TimerVirtual(TimerConfig(0, 20))
  val r = Module(new ReactionPurePrint())

  t.declareTriggeredReaction(r.t)

  reactions = Seq(r)
  localTimers = Seq(t)
  val timerIO = connectTimersAndCreateIO()

  reactorMain()
}
