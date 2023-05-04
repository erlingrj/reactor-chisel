package reactor.examples

import reactor._
import reactor.globals._

import chisel3._
import chisel3.util._

class ReactorCounter extends Reactor {
  class Reactor1IO extends ReactorIO {}


  val io = IO(new Reactor1IO)
  val r = Module(new ReactionPurePrint())
  val t = Module(new Timer(TimerConfig(0, 20)))

  val c = new PureConnectionBuilder()
  c << t
  c >> r.io.t
  c.construct()

  override val reactions = Seq(r)

  reactorMain
}
