package reactor

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer

import reactor.Reaction._
case class ReactionConfig(
                         nPrecedenceIn: Int = 0,
                         nPrecedenceOut: Int = 0
                         )
abstract class ReactionIO() extends Bundle {}

// FIXME: Hide behind global debug?
class ReactionStatusIO extends Bundle {
  val state = Output(UInt(4.W))
}

class ReactionPrecedencePorts(c: ReactionConfig) extends Bundle {
  val precedenceIn = Vec(c.nPrecedenceIn, new EventReadMaster(UInt(0.W), new PureToken))
  val precedenceOut = Vec(c.nPrecedenceOut, new EventWriteMaster(UInt(0.W), new PureToken))

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
  val precedenceIO = IO(new ReactionPrecedencePorts(c))
  val statusIO = IO(new ReactionStatusIO())

  val triggers: Seq[EventReadMaster[ _<: Data, _ <: Token[_<:Data]]] = Seq()
  val dependencies: Seq[EventReadMaster[ _<: Data, _ <: Token[_<:Data]]] = Seq()
  val antiDependencies: Seq[EventWriteMaster[ _<: Data, _ <: Token[_<:Data]]] = Seq()
  val states: Seq[StateReadWriteMaster[_ <: Data, _ <: Token[_<:Data]]] = Seq()

  val precedenceIn: Seq[EventReadMaster[UInt, PureToken]] = precedenceIO.precedenceIn.toSeq
  val precedenceOut: Seq[EventWriteMaster[UInt, PureToken]] = precedenceIO.precedenceOut.toSeq

  var reactionsAfter: ArrayBuffer[Reaction] = ArrayBuffer()
  var reactionsBefore: ArrayBuffer[Reaction] = ArrayBuffer()

  val logicalTag = RegInit(0.U(64.W))
  val physicalTag = RegInit(0.U(64.W))
  physicalTag := physicalTag + 1.U
  def driveDefaults(): Unit = {
    triggers.foreach(_.driveDefaults())
    dependencies.foreach(_.driveDefaults())
    antiDependencies.foreach(_.driveDefaults())
    precedenceIn.foreach(_.driveDefaults())
    precedenceOut.foreach(_.driveDefaults())
  }

  // Conditions to fire a reaction
  def fireReaction: Bool = {
      triggers.map(_.resp.valid).reduce(_ && _) &&
        dependencies.map(_.resp.valid).foldLeft(true.B)(_ && _)  &&
        precedenceIn.map(_.resp.valid).foldLeft(true.B)(_ && _)
  }

  // FIXME: Do operator overloading so we can do "r1 > r2 > r3 > r4`
  // Function for connecting a downstream precedence reaction.
  // The function is used like `upstream.precedes(downstream)`.
  var precedenceOutIdx = 0
  def precedes(down: Reaction): Unit = {
    require(precedenceOut.length > precedenceOutIdx, s"[Reaction.scala] Only ${precedenceOut.length} precedenceOut ports")

    // Create connection module for connecting the ports
    val connection = Module(new PureConnection(ConnectionConfig(
      gen1 = UInt(0.W),
      gen2 = new PureToken(),
      nChans = 1
    )))

    connection.io.write <> precedenceOut(precedenceOutIdx)

    down._isPrecededBy(this, connection.io.reads(0))

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
  private def _isPrecededBy(upstreamReaction: Reaction, upstreamPort: EventReadSlave[UInt, PureToken]): Unit = {
    require(precedenceIn.length > precedenceInIdx)
    precedenceIn(precedenceInIdx) <> upstreamPort
    reactionsBefore += upstreamReaction
  }

  def hasPresentTriggers: Bool = {
    triggers.map(_.resp.present).reduce(_ || _)
  }
  // FIXME: This function should return a Bool which is true when it is done
  def reactionBody: Unit

  val reactionEnable = WireDefault(false.B)
  val reactionDone = WireDefault(false.B)
  val regState = RegInit(sIdle)
  val regCycles = RegInit(0.U(32.W))


  // Updates the register containing the current logical tag based on the tag of the incoming events.
  def updateCurrentLogicalTag() = {
    for (t <- triggers) {
      when(t.resp.valid && t.resp.present) {
        logicalTag := t.resp.token.tag
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
        withReset(!reactionEnable) {
          reactionBody
        }
        when(reactionDone) {
          regState := sDone
        }
      }

      is(sDone) {
        antiDependencies.foreach(_.fire := true.B)
        precedenceOut.foreach(_.fire := true.B)
        triggers.foreach(_.fire := true.B)
        precedenceIn.foreach(_.fire := true.B)
        precedenceOut.foreach(_.fire := true.B)
        regState := sIdle

      }
    }
  }
  assert(!(regCycles > 200.U), "[Reaction] Reaction was running for over 200cc assumed error")

  // FIXME: These debug signals should be optional
  statusIO.state := regState
}

object Reaction {
  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
}