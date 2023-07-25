package reactor.examples

import chisel3._
import reactor._
import reactor.globals._

class ReactorCounterWithState extends Reactor {
  class Reactor1IO extends ReactorIO {
    def driveDefaultsFlipped() = {}
    def driveDefaults() = {}
  }

  val io = IO(new Reactor1IO)
  val t = new TimerTriggerVirtual(TimerConfig(Time.nsec(0), Time.nsec(20), false))
  localTriggers += t
  val r1 = Module(new ReactionCount(ReactionConfig(0,1)))
  val r2 = Module(new ReactionPrintCount(ReactionConfig(1,0)))
  val cnt = Module(new SingleValueState(StateConfig(defData, defToken, 2, Immediate)))

  r1 > r2

  t.declareTriggeredReaction(r1.io.t)
  t.declareTriggeredReaction(r2.io.t)
  cnt <> r1.stateIO.cnt
  cnt <> r2.stateIO.cnt

  reactions += r1
  reactions += r2
  val triggerIO = connectTimersAndCreateIO()

  reactorMain()
}
