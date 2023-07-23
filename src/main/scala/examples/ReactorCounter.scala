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
  val t = new TimerTriggerVirtual(TimerConfig(Time.nsec(0), Time.nsec(20), false))
  localTriggers += t
  val r = Module(new ReactionPurePrint())

  t.declareTriggeredReaction(r.t)

  reactions += (r)
  val triggerIO = connectTimersAndCreateIO()

  reactorMain()
}
