package reactor

import chisel3._
import fpgatidbits.PlatformWrapper._
import reactor.Reaction

abstract class ReactorIO extends Bundle {}
// FIXME: We need an optional precedence input port which should be connected to the first reaction of the reactor

abstract class Reactor extends Module {

  val io: ReactorIO
  val reactions: Seq[Reaction] = Seq()
  val inPorts: Seq[InputPort[_ <: Token]] = Seq()
  val outPorts: Seq[OutputPort[_ <: Token]] = Seq()
  val connections: Seq[Connection[_ <: Token]] = Seq()
  val childReactors: Seq[Reactor] = Seq()

  def reactorMain: Unit = {
    assert(util.PopCount(reactions.map(_.statusIO.state === Reaction.sRunning)) <= 1.U, "[Reactor.scala] Mutual exclusion between reactions not preserved")
  }
}

// FIXME: We want numMemPorts to be configurable
abstract class MainReactorIO(p: PlatformWrapperParams) extends GenericAcceleratorIF(AcceleratorParams(numMemPorts=0),p) {
  val start = Input(Bool())
  val done = Output(Bool())
  val running = Output(Bool())

  def connectScheduler(s: Scheduler): Unit = {
    s.io.start := start
    running := s.io.running
    done := s.io.done
  }
}

abstract class MainReactor(p: PlatformWrapperParams) extends GenericAccelerator(p) {

  val io: MainReactorIO
  val scheduler: Scheduler

}