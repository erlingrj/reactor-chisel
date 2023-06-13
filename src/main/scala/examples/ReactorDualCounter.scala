package reactor.examples

import reactor._
import reactor.globals._

import chisel3._
import chisel3.util._

class ReactorDualCounter extends Reactor {
  class Reactor1IO extends ReactorIO {
    def plugUnusedPorts(): Unit = {}
  }


  val io = IO(new Reactor1IO)
  val r1 = Module(new ReactionPurePrint(ReactionConfig(nPrecedenceOut = 1)))
  val r2 = Module(new ReactionPurePrint(ReactionConfig(nPrecedenceIn = 1)))
  val t1Cfg = TimerConfig(Time.nsec(0),Time.nsec(20))
  val t2Cfg = TimerConfig(Time.nsec(10),Time.nsec(20))
  val allTimerCfgs = Seq(t1Cfg, t2Cfg)
  val t1 = Module(new Timer(t1Cfg, allTimerCfgs.filterNot(_ == t1Cfg)))
  val t2 = Module(new Timer(t2Cfg, allTimerCfgs.filterNot(_ == t2Cfg)))


  val c1 = new PureConnectionFactory()
  c1 << t1.io.trigger
  c1 >> r1.io.t
  c1.construct()

  val c2 = new PureConnectionFactory()
  c2 << t2.io.trigger
  c2 >> r2.io.t
  c2.construct()

  r1 > r2

  reactions = Seq(r1, r2)

  val timerIO = connectTimersAndCreateIO()

  reactorMain()
}
