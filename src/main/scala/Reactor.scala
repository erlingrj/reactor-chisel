package reactor

import chisel3._
import chisel3.util.MixedVec
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

// Abstract base class for the external IO. These are IO signals e.g. connected to the pins of the FPGA
// Reactions can also read and write to such pins.
abstract class ReactorExternalIO(children: ArrayBuffer[Reactor]) extends Bundle {
  val childrenIO = MixedVec(Seq.tabulate(children.length)(i => children(i).externalIO.cloneType))
}

class ReactorTriggerIO(nLocalTriggers: Int, nContainedTriggers: Int) extends Bundle {
  val localTriggers = Vec(nLocalTriggers, new EventWriteSlave(0.U, new PureToken))
  val containedTriggers = Vec(nContainedTriggers, new EventWriteSlave(0.U, new PureToken))
}

abstract class Reactor extends Module {

  // The Inputs and Outputs of the reactor
  val io: ReactorIO
  // The trigger (Timers) inputs to the reactor. All Timer triggers are generated in the same external module
  val triggerIO: ReactorTriggerIO
  // The external (input/output with @physical attribute) ports which can be read and written from reactions.
  val externalIO: ReactorExternalIO

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
    connectExternalIOInternally()
    fixNaming()
  }

  def fixNaming(): Unit = {
    childReactors.foreach(r => {
      r.suggestName(r.name)
    })
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

  // This convenience function connects the externalIO bundle internally to the child reactors
  def connectExternalIOInternally(): Unit = {
    for ((extIO, child) <- externalIO.childrenIO zip childReactors) {
      extIO <> child.externalIO
    }
  }
}

