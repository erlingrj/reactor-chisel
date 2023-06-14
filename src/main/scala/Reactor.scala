package reactor

import chisel3._
import fpgatidbits.PlatformWrapper._
import reactor.Reaction

package object globals {
  import Time._
  val defData = UInt(8.W)
  val defToken = new SingleToken(defData)
  val pureData = UInt(8.W)

}

abstract class ReactorIO extends Bundle {

  // All the ReactorIO implementations must provide a driveDefaults function which can "plug" any ununsed ports
  def plugUnusedPorts(): Unit
}
// FIXME: We need an optional precedence input port which should be connected to the first reaction of the reactor


class ReactorTimerIO(nLocalTimers: Int, nContainedTimers: Int) extends Bundle {
  val localTimers = Vec(nLocalTimers, new EventWriteSlave(0.U, new PureToken))
  val containedTimers = Vec(nContainedTimers, new EventWriteSlave(0.U, new PureToken))
}

abstract class Reactor extends Module {

  val io: ReactorIO
  val timerIO: ReactorTimerIO

  // FIXME: Verify that there is a precedence relationship among all reactions, i.e. mutex is guaranteed
  // FIXME: These vars should maybe be prependend with _
  var reactions: Seq[Reaction] = Seq()
  var inPorts: Seq[InputPort[_ <: Data, _ <: Token[_<: Data]]] = Seq()
  var outPorts: Seq[OutputPort[_ <: Data, _ <: Token[_<: Data]]] = Seq()
  var connections: Seq[Connection[_ <: Data,_ <: Token[_<: Data]]] = Seq()
  var childReactors: Seq[Reactor] = Seq()
  var localTimers: Seq[TimerVirtual] = Seq()
  var containedTimers: Seq[TimerVirtual] = Seq()
  var states: Seq[State[_ <: Data, _ <: Token[_ <: Data]]] = Seq()


  def reactorMain(): Unit = {
    assert(util.PopCount(reactions.map(_.statusIO.state === Reaction.sRunning)) <= 1.U, "[Reactor.scala] Mutual exclusion between reactions not preserved")
  }

  def connectTimersAndCreateIO(): ReactorTimerIO = {
    // Create the seq of contained virtual timers. Also create the Seq of TimerIO which matches the containedTimers.
    // It is important they they match. Because the top-level Reactor will use containedTimers to find the
    // needed timer configs (offset and period).
    containedTimers = (for (child <- childReactors) yield child.localTimers ++ child.containedTimers).flatten
    val containedTimersIO = (for (child <- childReactors) yield child.timerIO.localTimers ++ child.timerIO.containedTimers).flatten

    // Create the timerIO
    val timerIO = IO(new ReactorTimerIO(localTimers.size, containedTimers.size))

    // Connect local timers and construct the connections
    for ((timer, i) <- localTimers.zipWithIndex) {
      timer.declareInputPort(timerIO.localTimers(i))
      timer.construct()
    }

    // Forward the timerIO to the contained timers
    for ((containedTimerIO, i) <- containedTimersIO.zipWithIndex) {
      containedTimerIO <> timerIO.containedTimers(i)
    }

    // Return the newly created ReactorTimerIO.
    timerIO
  }
}


class StandaloneMainReactor(mainReactorGenFunc: () => Reactor) extends Module {
  val mainReactor = Module(mainReactorGenFunc())
  val mainTimer = Module(new MainTimer(mainReactor))

  // Connect the mainTimer to the mainReactor
  val mainReactorTimerIO = mainReactor.timerIO.localTimers ++ mainReactor.timerIO.containedTimers
  for ((mainTimer, reactorTimer) <- mainTimer.io.timers zip mainReactorTimerIO) {
    reactorTimer <> mainTimer.trigger
  }

  mainTimer.io.tagAdvanceGrant := Tag.FOREVER

  // Plug any top-level
  mainReactor.io.plugUnusedPorts()
}

// FIXME: We want numMemPorts to be configurable
abstract class CodesignMainReactorIO(p: PlatformWrapperParams) extends GenericAcceleratorIF(AcceleratorParams(numMemPorts=0),p) {
  val start = Input(Bool())
  val done = Output(Bool())
  val running = Output(Bool())

  def connectScheduler(s: Scheduler): Unit = {
    s.io.start := start
    running := s.io.running
    done := s.io.done
  }
}

abstract class CodesignMainReactor(p: PlatformWrapperParams) extends GenericAccelerator(p) {

  val io: CodesignMainReactorIO
  val scheduler: Scheduler

}