package reactor

import chisel3._
import fpgatidbits.PlatformWrapper._
import reactor.Reaction

package object globals {
  val defData = UInt(8.W)
  val defToken = new SingleToken(defData)
  val pureData = UInt(8.W)

}

abstract class ReactorIO extends Bundle {}
// FIXME: We need an optional precedence input port which should be connected to the first reaction of the reactor

abstract class Reactor extends Module {

  val io: ReactorIO
  // FIXME: Verify that there is a precedence relationship among all reactions, i.e. mutex is guaranteed
  val reactions: Seq[Reaction] = Seq()
  val inPorts: Seq[InputPort[_ <: Data, _ <: Token[_<: Data]]] = Seq()
  val outPorts: Seq[OutputPort[_ <: Data, _ <: Token[_<: Data]]] = Seq()
  val connections: Seq[Connection[_ <: Data,_ <: Token[_<: Data]]] = Seq()
  val childReactors: Seq[Reactor] = Seq()
  val states: Seq[State[_ <: Data, _ <: Token[_ <: Data]]] = Seq()

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