package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror

abstract class ReactorElement(val id: String) extends Module {

}

class EventQueueElement(implicit rc: ReactorGlobalParams) extends Bundle {
  val ts = UInt(rc.numClkBits.W)
}

class ReactorIO(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Bundle {
  val logicalTime = Input(UInt(rc.numClkBits.W))

  val running = Output(Bool())
  val eventQueueHead = Output(Valid(new EventQueueElement))

  val inPorts = MixedVec(for (inPort <- c.inPorts) yield Decoupled(inPort.gen))
  val outPorts = MixedVec(for (outPort <- c.inPorts) yield Decoupled(outPort.gen))

  def get(name: String): DecoupledIO[_<:Data] = {
    // Search through inPorts

    for ((c, i) <- c.inPorts.zipWithIndex) {
      if (c.id == name) return inPorts(i)
    }

    for ((c, i) <- c.outPorts.zipWithIndex) {
      if (c.id == name) return outPorts(i)
    }

    // If function has not returned yet, throw assertion
    assert(false.B, s"[Reactor.scala] ReactorIO.get did not find port with name= $name")
    WireInit(Decoupled(0.U))
  }
}

abstract class Reactor(override val id: String, val top: Boolean, c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends ReactorElement(id) {
  val io = IO(new ReactorIO(c))

  // Clocks
  val physicalTime = RegInit(0.U(rc.numClkBits.W))
  val logicalTime = RegInit(0.U(rc.numClkBits.W))

  //
  val actions = (for (a <- c.actions) yield Module(new Action(a)))
  val timers = (for (t <- c.timers) yield Module(new Timer(t)))
  def reactions: Seq[Reaction]
  def reactors: Seq[Reactor]

  def getElement(name: String): ReactorElement = {
    // Search through inPorts
    for ((a, i) <- c.actions.zipWithIndex) {
      if (a.id == name) return actions(i)
    }

    for ((c, i) <- c.timers.zipWithIndex) {
      if (c.id == name) return timers(i)
    }

    for ((c, i) <- c.reactions.zipWithIndex) {
      if (c.id == name) return reactions(i)
    }
    for ((c, i) <- c.reactors.zipWithIndex) {
      if (c.id == name) return reactors(i)
    }

    // If function has not returned yet, throw assertion
    assert(false.B, s"[Reaction.scala] ReactionIO.get did not find port with name= $name")
    new Timer(new TimerConfig("null", 0, 0, new TimerPortConfig("null")))
  }

  // Make connections. A little clunky since we must first find the connectionType and then
  //  re-interpret the ReactorElement at each side of the connection as the appropriate class
  //  Could we improve this?
  for (conn <- c.connections) {
    val dstId = conn.dstComponent.id
    val dstPortId = conn.dstPort.id
    val srcId = conn.srcComponent.id
    val srcPortId = conn.srcPort.id

    if (conn.connectionType == new ActionReactionConnection) {
      val dst = getElement(dstId).asInstanceOf[Reaction]
      val dstPort = dst.io.get(dstPortId)

      val src = getElement(srcId).asInstanceOf[Action[_<:Data]]
      val srcPort = src.io.outPort

      require(dstPort.getClass == srcPort.getClass,
        s"[Reactor] Tried to connect ports ${srcPortId} and $dstPortId of non matching type")
      require(DataMirror.directionOf(dstPort) == ActualDirection.Input,
        s"[Reactor] Src port $srcPortId is not output")
      require(DataMirror.directionOf(srcPort) == ActualDirection.Output,
        s"[Reactor] Dst port $dstPortId is not input")

      // Connect
      dstPort <> srcPort
    } else if (conn.connectionType == new ReactionActionConnection) {

      val dst = getElement(dstId).asInstanceOf[Action[_<:Data]]
      val dstPort = dst.io.schedule

      val src = getElement(srcId).asInstanceOf[Reaction]
      val srcPort = src.io.get(srcPortId)

      require(dstPort.getClass == srcPort.getClass,
        s"[Reactor] Tried to connect ports ${srcPortId} and $dstPortId of non matching type")
      require(DataMirror.directionOf(dstPort) == ActualDirection.Input,
        s"[Reactor] Src port $srcPortId is not output")
      require(DataMirror.directionOf(srcPort) == ActualDirection.Output,
        s"[Reactor] Dst port $dstPortId is not input")

      // Connect
      dstPort <> srcPort

    } else if (conn.connectionType == new TimerReactionConnection) {

      val dst = getElement(dstId).asInstanceOf[Reaction]
      val dstPort = dst.io.get(dstPortId)

      val src = getElement(srcId).asInstanceOf[Timer[_<:Data]]
      val srcPort = src.io.outPort

      require(dstPort.getClass == srcPort.getClass,
        s"[Reactor] Tried to connect ports ${srcPortId} and $dstPortId of non matching type")
      require(DataMirror.directionOf(dstPort) == ActualDirection.Input,
        s"[Reactor] Src port $srcPortId is not output")
      require(DataMirror.directionOf(srcPort) == ActualDirection.Output,
        s"[Reactor] Dst port $dstPortId is not input")

      // Connect
      dstPort <> srcPort
    } else if (conn.connectionType == new ReactorReactionConnection) {

      val dst = getElement(dstId).asInstanceOf[Reaction]
      val dstPort = dst.io.get(dstPortId)

      val srcPort = io.get(srcPortId)

      require(dstPort.getClass == srcPort.getClass,
        s"[Reactor] Tried to connect ports ${srcPortId} and $dstPortId of non matching type")
      require(DataMirror.directionOf(dstPort) == ActualDirection.Input,
        s"[Reactor] Src port $srcPortId is not output")
      require(DataMirror.directionOf(srcPort) == ActualDirection.Output,
        s"[Reactor] Dst port $dstPortId is not input")

      // Connect
      dstPort <> srcPort
    } else if (conn.connectionType == new ReactionReactorConnection) {
      // A Reaction connected to:
      //  A) An OUTPUT port of this Reactor (which will be connected to another Reactor)
      //  B) An INPUT port of a contained Reactor

      val dstPort = if (dstId == c.id) {
        io.get(dstPortId)
      } else {
        val dst = getElement(dstId).asInstanceOf[Reactor]
        dst.io.get(dstPortId)
      }

      val srcPort = io.get(srcPortId)

      require(dstPort.getClass == srcPort.getClass,
        s"[Reactor] Tried to connect ports ${srcPortId} and $dstPortId of non matching type")
      require(DataMirror.directionOf(dstPort) == ActualDirection.Input,
        s"[Reactor] Src port $srcPortId is not output")
      require(DataMirror.directionOf(srcPort) == ActualDirection.Output,
        s"[Reactor] Dst port $dstPortId is not input")

      // Connect
      dstPort <> srcPort
    } else if (conn.connectionType == new ReactorReactorConnection) {
      // Following scenarios:
      //  A) Input Port of THIS connected to Input port of contained
      //  B) Output Port of Contained <> Output Port of This
      //  C) Output Port of COntained <> Input Port of Contained


      val srcPort = if (srcId == c.id) {
        // A)
        io.get(srcPortId)
      } else {
        // B) and C)
        val src = getElement(srcId).asInstanceOf[Reactor]
        src.io.get(srcPortId)
      }

      val dstPort = if (dstId == c.id) {
        // B)
        io.get(dstPortId)
      } else {
        // A) and C)
        val dst = getElement(dstId).asInstanceOf[Reactor]
        dst.io.get(dstPortId)
      }

      require(dstPort.getClass == srcPort.getClass,
        s"[Reactor] Tried to connect ports ${srcPortId} and $dstPortId of non matching type")
      require(DataMirror.directionOf(dstPort) == ActualDirection.Input,
        s"[Reactor] Src port $srcPortId is not output")
      require(DataMirror.directionOf(srcPort) == ActualDirection.Output,
        s"[Reactor] Dst port $dstPortId is not input")

      // Connect
      dstPort <> srcPort
    }
  }
}