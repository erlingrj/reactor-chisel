package reactor

import chisel3._
import chisel3.experimental.DataMirror
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
  // All the ReactorIO implementations must provide a driveDefaults function which can "plug" any ununsed ports
  // Both from the inside and the outside (flipped)
  def driveDefaultsFlipped(): Unit
  def driveDefaults(): Unit
}

class ReactorStatusIO extends Bundle {
  val idle = Output(Bool())
}
// FIXME: We need an optional precedence input port which should be connected to the first reaction of the reactor

// Abstract base class for the external IO. These are IO signals e.g. connected to the pins of the FPGA
// Reactions can also read and write to such pins.
abstract class ReactorExternalIO(children: ArrayBuffer[Reactor]) extends Bundle {
  val childrenIO = MixedVec(Seq.tabulate(children.length)(i => children(i).externalIO.cloneType))
}

abstract class ReactorPhysicalIO(children: ArrayBuffer[Reactor]) extends Bundle {
  val childrenIO = MixedVec(Seq.tabulate(children.length)(i => children(i).physicalIO.cloneType))

  private def flatten(data: Data): Seq[Data] = {
    data match {
      case d: MixedVec[_] =>
        d.flatMap(flatten).toSeq
      case d: ReactorPhysicalIO => d.getElements.flatMap(flatten).toSeq
      case d: Data => Seq(d)
      case _ =>
        require(false, s"Got ${data}")
        Seq(UInt())
    }
  }

  def getAllPorts: ArrayBuffer[Data] = {
    val elts = getElements
    val flattend: ArrayBuffer[Data] = ArrayBuffer()
    for (elem <- elts) {
      flatten(elem).foreach(flattend += _)
    }
    flattend
  }

}

class ReactorPhysicalFlippedIO(org: ReactorPhysicalIO) extends Bundle {

  println(org.getAllPorts)

  val ports: MixedVec[EventWriteSlave[_ <: Data, _ <: Token[_<:Data]]] = MixedVec(Seq.tabulate(org.getAllPorts.size)( i =>
    org.getAllPorts(i) match {
      case p: EventPureReadMaster => new EventPureWriteSlave
      case p: EventSingleValueReadMaster[_] => new EventSingleValueWriteSlave(p.genData)
      case p: EventArrayReadMaster[_] => new EventArrayWriteSlave(p.genData, p.genToken)
      case _ =>
        require(false, s"ReactorPhysicalIO had a port of unrecognized type: ${org.getAllPorts(i)}")
        new EventPureWriteSlave
    }))

  def getAllPorts: Seq[Data] = this.getElements.head.asInstanceOf[MixedVec[Data]].toSeq
}


case class NumTriggers(local: Int, contained: Int)

case class ReactorTriggerConfig(
    timers: NumTriggers,
)

class ReactorTriggerIO(cfg: ReactorTriggerConfig) extends Bundle {
  val localTimerTriggers = Vec(cfg.timers.local, new EventPureWriteSlave)
  val containedTimerTriggers = Vec(cfg.timers.contained, new EventPureWriteSlave)

  def allTimerTriggers = localTimerTriggers ++ containedTimerTriggers
}

abstract class Reactor extends Module {

  // The Inputs and Outputs of the reactor
  val io: ReactorIO
  // The trigger (Timers) inputs to the reactor. All Timer triggers are generated in the same external module
  val triggerIO: ReactorTriggerIO
  // The external (input/output with @external attribute) ports which can be read and written from reactions.
  val externalIO: ReactorExternalIO

  // The physical io are inputs arising from physical actions
  val physicalIO: ReactorPhysicalIO

  val statusIO = IO(new ReactorStatusIO)

  // FIXME: These vars should maybe be prependend with _
  var reactions: ArrayBuffer[Reaction] = new ArrayBuffer()
  var inPorts: ArrayBuffer[InputPort[_ <: Data, _ <: Token[_<: Data]]] = new ArrayBuffer()
  var outPorts: ArrayBuffer[OutputPort[_ <: Data, _ <: Token[_<: Data]]] = new ArrayBuffer()
  var connections: ArrayBuffer[Connection[_ <: Data,_ <: Token[_<: Data]]] = new ArrayBuffer()
  var childReactors: ArrayBuffer[Reactor] = new ArrayBuffer
  var localTriggers: ArrayBuffer[TriggerPureVirtual] = new ArrayBuffer()
  var containedTriggers: ArrayBuffer[TriggerPureVirtual] = new ArrayBuffer()
  var states: ArrayBuffer[State[_ <: Data, _ <: Token[_ <: Data]]] = new ArrayBuffer()
  var unconnectedChildInPorts: ArrayBuffer[UnconnectedInputPort[_ <: Data, _ <: Token[_ <: Data]]] = new ArrayBuffer()

  // To fetch the Physical Action triggers, we simply create a Pure Virtual Trigger for each IO in the physicalIO bundle.
  def phyTriggers = physicalIO.getAllPorts.map(_ => new PhysicalActionTriggerPureVirtual(offset = Time.nsec(0)))

  // Is the current Reactor (and any contained reactor idle?)
  def isIdle(): Bool = {
    ( childReactors.map(_.statusIO.idle) ++
      inPorts.map(!_.io.outward.resp.valid)
      ).reduceOption(_ && _).getOrElse(true.B)
  }

  def reactorMain(): Unit = {
    statusIO.idle := isIdle()
    assert(util.PopCount(reactions.map(_.statusIO.state === Reaction.sRunning)) <= 1.U, "[Reactor.scala] Mutual exclusion between reactions not preserved")
    connectExternalIOInternally()
    connectPhysicalIOInternally()
    driveUnconnectedPorts()
    fixNaming()
  }

  def fixNaming(): Unit = {
    childReactors.foreach(r => {
      r.suggestName(r.name)
    })
  }

  def allTriggerConfigs(): ArrayBuffer[TriggerPureVirtual] = localTriggers ++ containedTriggers ++ phyTriggers

  def connectTimersAndCreateIO(): ReactorTriggerIO = {
    // Create the seq of contained virtual timers. Also create the Seq of TimerIO which matches the containedTimers.
    // It is important they they match. Because the top-level Reactor will use containedTimers to find the
    // needed timer configs (offset and period).
    containedTriggers = (for (child <- childReactors) yield child.localTriggers ++ child.containedTriggers).flatten
    val containedTimersIO = (for (child <- childReactors) yield child.triggerIO.localTimerTriggers ++ child.triggerIO.containedTimerTriggers).flatten

    println(s"Reactor ${this.name} has ${localTriggers.size} local timers ${containedTriggers.size} contained timers.")

    // Create the timerIO
    val timerIO = IO(new ReactorTriggerIO(
      ReactorTriggerConfig(
        timers = NumTriggers(localTriggers.size, containedTriggers.size),
      )
    ))

    // Connect local timers and construct the connections
    for ((timer, i) <- localTriggers.zipWithIndex) {
      timer.declareInputPort(timerIO.localTimerTriggers(i))
      timer.construct().foreach(inp => inPorts += inp) // Construct the inputPorts for the timer triggers and add them
    }


    // Forward the timerIO to the contained timers
    for ((containedTimerIO, i) <- containedTimersIO.zipWithIndex) {
      containedTimerIO <> timerIO.containedTimerTriggers(i)
    }

    // Return the newly created ReactorTimerIO.
    timerIO
  }

  def driveUnconnectedPorts(): Unit = {

    // Find any unconnected input port
    for (port <- unconnectedChildInPorts) {
      port.io.writeAbsent := false.B
      // If we have any triggers, use a trigger to know when to write absent tokens into the unconnected ports.
      if (triggerIO.allTimerTriggers.nonEmpty) {
        val trig = triggerIO.allTimerTriggers.head
        when(trig.fire) {
          port.io.writeAbsent := true.B
        }
      } else if (inPorts.nonEmpty) {
        // If not triggers, use an input port to know
        val trig = inPorts.head
        // Write an absent token only once, at the first cycle when we have a token on the inport
        port.io.writeAbsent := trig.io.inward.head.resp.valid && !RegNext(trig.io.inward.head.resp.valid)
      } else {
        require(false, "Reactor has no way of knowing when to trigger unconnected port")
      }
    }
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

  def connectPhysicalIOInternally(): Unit ={
    for ((phyIO, child) <- physicalIO.childrenIO zip childReactors) {
      phyIO <> child.physicalIO
    }
  }
}

