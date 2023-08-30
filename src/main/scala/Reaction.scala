package reactor

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer

import reactor.Reaction._
case class ReactionConfig(
                         nPrecedenceIn: Int = 0,
                         nPrecedenceOut: Int = 0
                         )
abstract class ReactionIO() extends Bundle {
  /**
   * Drive all the IOs of the Reaction to default inactive values.
   */
  def driveDefaults(): Unit = {
    for (elt <- this.getElements) {
      elt match {
        case value: TokenReadMaster[_,_] => value.driveDefaults()
        case value: TokenWriteMaster[_,_] => value.driveDefaults()
        case value: StateReadWriteMaster[_,_] => value.driveDefaults()
        case _ =>
      }
    }
  }
}
abstract class ReactionStateIO() extends Bundle {

  // This function iterates through all the elements of the bundle, casts them the StateReadWriteMaster
  // and drives the default values.
  def driveDefaults(): Unit = {
    for(elt <- this.getElements) {
      elt.asInstanceOf[StateReadWriteMaster[_ <: Data, _ <: Token[_ <: Data]]].driveDefaults()
    }
  }
}


abstract class ExternalIO() extends Bundle {

}

// FIXME: Hide behind global debug?
class ReactionStatusIO extends Bundle {
  val state = Output(UInt(4.W))
}

class ReactionPrecedencePorts(c: ReactionConfig) extends Bundle {
  val precedenceIn = Vec(c.nPrecedenceIn, new PureTokenReadMaster)
  val precedenceOut = Vec(c.nPrecedenceOut, new PureTokenWriteMaster)

  def driveDefaults() = {
    precedenceIn.foreach(_.driveDefaults())
    precedenceOut.foreach(_.driveDefaults())
  }
}

class ReactionStatePorts()

abstract class Reaction (val c: ReactionConfig = ReactionConfig(0,0)) extends Module {
  import ReactionApi.{lf_set, lf_get, lf_present}
  implicit val instance: Reaction = this
  val io: ReactionIO
  val stateIO: ReactionStateIO

  val precedenceIO = IO(new ReactionPrecedencePorts(c))
  val statusIO = IO(new ReactionStatusIO())

  val triggers: Seq[TokenReadMaster[ _<: Data, _ <: Token[_<:Data]]] = Seq()
  val dependencies: Seq[TokenReadMaster[ _<: Data, _ <: Token[_<:Data]]] = Seq()
  val antiDependencies: Seq[TokenWriteMaster[ _<: Data, _ <: Token[_<:Data]]] = Seq()
  val states: Seq[StateReadWriteMaster[_ <: Data, _ <: Token[_<:Data]]] = Seq()

  val precedenceIn: Seq[TokenReadMaster[UInt, PureToken]] = precedenceIO.precedenceIn.toSeq
  val precedenceOut: Seq[TokenWriteMaster[UInt, PureToken]] = precedenceIO.precedenceOut.toSeq

  var reactionsAfter: ArrayBuffer[Reaction] = ArrayBuffer()
  var reactionsBefore: ArrayBuffer[Reaction] = ArrayBuffer()

  val logicalTag = RegInit(0.U(64.W))
  val physicalTag = RegInit(0.U(64.W))
  physicalTag := physicalTag + 1.U
  def driveDefaults(): Unit = {
    io.driveDefaults()
    precedenceIn.foreach(_.driveDefaults())
    precedenceOut.foreach(_.driveDefaults())
    stateIO.driveDefaults()
  }

  // Conditions to fire a reaction
  def fireReaction: Bool = {
      triggers.map(_.token).reduce(_ && _) &&
        dependencies.map(_.token).foldLeft(true.B)(_ && _)  &&
        precedenceIn.map(_.token).foldLeft(true.B)(_ && _) &&
        antiDependencies.map(_.req.ready).foldLeft(true.B)(_ && _)
  }

  // Function for connecting a downstream precedence reaction.
  // The function is used like `upstream.precedes(downstream)`.
  var precedenceOutIdx = 0
  def precedes(down: Reaction): Unit = {
    require(precedenceOut.length > precedenceOutIdx, s"[Reaction.scala] Precedence connection failed, only ${precedenceOut.length} precedenceOut ports")

    // Create connection module for connecting the ports
    val precedenceConn = Module(new PureConnection(ConnectionConfig(
      genData = UInt(0.W),
      genToken = new PureToken(),
      nChans = 1
    )))

    precedenceConn.io.write <> precedenceOut(precedenceOutIdx)

    down._isPrecededBy(this, precedenceConn.io.reads(0))

    // Store a reference to this downstream reaction
    reactionsAfter += down

    precedenceOutIdx += 1
  }

  def > (down: Reaction): Reaction = {
    precedes(down)
    down
  }
  // This is a private API for connecting upstream/downstream precedence reactions
  // The user uses `upstream.precedes(downstream)` and internally the upstream
  // makes a `_isPrecededBy
  var precedenceInIdx = 0
  private def _isPrecededBy(upstreamReaction: Reaction, upstreamPort: TokenReadSlave[UInt, PureToken]): Unit = {
    require(precedenceIn.length > precedenceInIdx)
    precedenceIn(precedenceInIdx) <> upstreamPort
    reactionsBefore += upstreamReaction
  }

  def hasPresentTriggers: Bool = {
    triggers.map(_.present).reduce(_ || _)
  }

  // This is the user-supplied reaction body
  def reactionBody(): Unit

  val reactionDone = WireDefault(true.B)
  val reactionEnable = WireDefault(false.B)
  val regState = RegInit(sIdle)
  val regCycles = RegInit(0.U(32.W))


  // Updates the register containing the current logical tag based on the tag of the incoming events.
  def updateCurrentLogicalTag() = {
    for (t <- triggers) {
      when(t.token && t.present) {
        logicalTag := t.tag
      }
    }
  }
  def reactionMain(): Unit = {
    require(triggers.length > 0, "[Reaction.scala] Reaction has no triggers")
    driveDefaults()

    switch(regState) {
      is(sIdle) {
        regCycles := 0.U

        when(fireReaction) {
          updateCurrentLogicalTag()
          when(hasPresentTriggers) {
            regState := sRunning
          }.otherwise {
            regState := sDone
          }
        }
      }

      is(sRunning) {
        regCycles := regCycles + 1.U
        reactionEnable := true.B
        reactionDone := true.B
        withReset(!reactionEnable) {
          reactionBody()
        }
        when(reactionDone) {
          regState := sDone
        }
      }

      is(sDone) {
        // Send tokens (absent/present depending on whether they were written to) to all outgoing channels
        antiDependencies.foreach{ f =>
          f.fire := true.B
          f.tag := logicalTag
        }
        triggers.foreach(_.fire := true.B) // Consume tokens from all triggers
        precedenceIn.foreach(_.fire := true.B) // Consume tokens from all precedence inputs
        precedenceOut.foreach(_.writeAbsent()) // Write absence tokens to precedence ports. We dont want to trigger downstream just enable
        regState := sIdle

      }
    }
  }

  assert(!(regCycles > 10000.U), "[Reaction] Reaction was running for over 10000cc assumed error")

  // FIXME: These debug signals should be optional
  statusIO.state := regState
}

object Reaction {
  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
}