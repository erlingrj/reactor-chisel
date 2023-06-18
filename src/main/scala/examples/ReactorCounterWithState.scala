package reactor.examples

import chisel3._
import reactor._
import reactor.globals._

class ReactorCounterWithState extends Reactor {
  class Reactor1IO extends ReactorIO {
    def plugUnusedPorts() = {}
  }

  val io = IO(new Reactor1IO)
  val t = new TimerVirtual(TimerConfig(Time.nsec(0), Time.nsec(20)))
  localTimers += t
  val r1 = Module(new ReactionCount(ReactionConfig(0,1)))
  val r2 = Module(new ReactionPrintCount(ReactionConfig(1,0)))
  val cnt = Module(new SingleValueState(StateConfig(defData, defToken, 2, Immediate)))

  r1 > r2

  t.declareTriggeredReaction(r1.io.t)
  t.declareTriggeredReaction(r2.io.t)
  cnt <> r1.stateIO.cnt
  cnt <> r2.stateIO.cnt

  reactions = Seq(r1,r2)
  val timerIO = connectTimersAndCreateIO()

  reactorMain()
}
