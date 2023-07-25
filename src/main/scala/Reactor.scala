package reactor

import chisel3._
import fpgatidbits.PlatformWrapper._
import reactor.Reaction

import scala.collection.mutable.ArrayBuffer

package object globals {
  import Time._
  val defData = UInt(8.W)
  val defToken = new SingleToken(defData)
  val pureData = UInt(8.W)

}

case class GlobalReactorConfig(
                              timeout: Time
                              )

abstract class ReactorIO extends Bundle {
  val idle = Output(Bool())

  // All the ReactorIO implementations must provide a driveDefaults function which can "plug" any ununsed ports
  // Both from the inside and the outside (flipped)
  def driveDefaultsFlipped(): Unit
  def driveDefaults(): Unit
}
// FIXME: We need an optional precedence input port which should be connected to the first reaction of the reactor


class ReactorTriggerIO(nLocalTriggers: Int, nContainedTriggers: Int) extends Bundle {
  val localTriggers = Vec(nLocalTriggers, new EventWriteSlave(0.U, new PureToken))
  val containedTriggers = Vec(nContainedTriggers, new EventWriteSlave(0.U, new PureToken))
}

abstract class Reactor extends Module {

  val io: ReactorIO
  val triggerIO: ReactorTriggerIO

  // FIXME: Verify that there is a precedence relationship among all reactions, i.e. mutex is guaranteed
  // FIXME: These vars should maybe be prependend with _
  var reactions: ArrayBuffer[Reaction] = new ArrayBuffer()
  var inPorts: ArrayBuffer[InputPort[_ <: Data, _ <: Token[_<: Data]]] = new ArrayBuffer()
  var outPorts: ArrayBuffer[OutputPort[_ <: Data, _ <: Token[_<: Data]]] = new ArrayBuffer()
  var connections: ArrayBuffer[Connection[_ <: Data,_ <: Token[_<: Data]]] = new ArrayBuffer()
  var childReactors: ArrayBuffer[Reactor] = new ArrayBuffer
  var localTriggers: ArrayBuffer[TimerTriggerVirtual] = new ArrayBuffer()
  var containedTriggers: ArrayBuffer[TimerTriggerVirtual] = new ArrayBuffer()
  var states: ArrayBuffer[State[_ <: Data, _ <: Token[_ <: Data]]] = new ArrayBuffer()

  // Is the current Reactor (and any contained reactor idle?)
  def isIdle(): Bool = {
    ( childReactors.map(_.io.idle) ++
      inPorts.map(!_.io.outward.resp.valid)
      ).reduceOption(_ && _).getOrElse(true.B)
  }

  def reactorMain(): Unit = {
    io.idle := isIdle()
    assert(util.PopCount(reactions.map(_.statusIO.state === Reaction.sRunning)) <= 1.U, "[Reactor.scala] Mutual exclusion between reactions not preserved")
  }

  def connectTimersAndCreateIO(): ReactorTriggerIO = {
    // Create the seq of contained virtual timers. Also create the Seq of TimerIO which matches the containedTimers.
    // It is important they they match. Because the top-level Reactor will use containedTimers to find the
    // needed timer configs (offset and period).
    containedTriggers = (for (child <- childReactors) yield child.localTriggers ++ child.containedTriggers).flatten
    val containedTimersIO = (for (child <- childReactors) yield child.triggerIO.localTriggers ++ child.triggerIO.containedTriggers).flatten

    println(s"Reactor ${this.name} has ${localTriggers.size} local timers ${containedTriggers.size} contained timers.")

    // Create the timerIO
    val timerIO = IO(new ReactorTriggerIO(localTriggers.size, containedTriggers.size))

    // Connect local timers and construct the connections
    for ((timer, i) <- localTriggers.zipWithIndex) {
      timer.declareInputPort(timerIO.localTriggers(i))
      timer.construct().foreach(inp => inPorts += inp) // Construct the inputPorts for the timer triggers and add them
    }


    // Forward the timerIO to the contained timers
    for ((containedTimerIO, i) <- containedTimersIO.zipWithIndex) {
      containedTimerIO <> timerIO.containedTriggers(i)
    }

    // Return the newly created ReactorTimerIO.
    timerIO
  }

  // TODO: It would be great to connect states to the reactions ReactionStateIO automatically, by matching names
  // or even just positions.
  def connectState(): Unit = {

  }
}

class StandaloneMainReactorIO extends Bundle {
  val terminate = Output(Bool())
}

class StandaloneMainReactor(mainReactorGenFunc: () => Reactor)(implicit globalCfg: GlobalReactorConfig) extends Module {
  val io = IO(new StandaloneMainReactorIO)
  val mainReactor = Module(mainReactorGenFunc())
  val triggerGenerator = Module(new MainTriggerGenerator(mainReactor))

  // Connect the triggerGenerator to the mainReactor
  val mainReactorTriggerIO = mainReactor.triggerIO.localTriggers ++ mainReactor.triggerIO.containedTriggers
  for ((triggerGen, reactorTrigger) <- triggerGenerator.io.timers zip mainReactorTriggerIO) {
    reactorTrigger <> triggerGen.trigger
  }

  triggerGenerator.io.tagAdvanceGrant := Tag.FOREVER
  triggerGenerator.io.mainReactorIdle := mainReactor.io.idle
  // Plug any top-level
  mainReactor.io.driveDefaultsFlipped()

  io.terminate := triggerGenerator.io.terminate
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