package reactor

import chisel3._
import chisel3.util._


case class ReactionConfig(
                         nPrecedenceIn: Int,
                         nPrecedenceOut: Int
                         )
abstract class ReactionIO() extends Bundle {}

class ReactionPrecedencePorts(c: ReactionConfig) extends Bundle {
  val precedenceIn = Vec(c.nPrecedenceIn, new EventReadMaster(new PureToken))
  val precedenceOut = Vec(c.nPrecedenceOut, new EventWriteMaster(new PureToken))

  def driveDefaults() = {
    precedenceIn.foreach(_.driveDefaults())
    precedenceOut.foreach(_.driveDefaults())
  }
}
abstract class Reaction(val c: ReactionConfig = ReactionConfig(0,0)) extends Module {
  val io: ReactionIO
  val precedenceIO = IO(new ReactionPrecedencePorts(c))

  val triggers: Seq[EventReadMaster[_ <: Token]] = Seq()
  val dependencies: Seq[EventReadMaster[_ <: Token]] = Seq()
  val antiDependencies: Seq[EventWriteMaster[_ <: Token]] = Seq()
  val precedenceIn: Seq[EventReadMaster[PureToken]] = precedenceIO.precedenceIn.toSeq
  val precedenceOut: Seq[EventWriteMaster[PureToken]] = precedenceIO.precedenceOut.toSeq


  def driveDefaults(): Unit = {
    triggers.foreach(_.driveDefaults())
    dependencies.foreach(_.driveDefaults())
    antiDependencies.foreach(_.driveDefaults())
    precedenceIn.foreach(_.driveDefaults())
    precedenceOut.foreach(_.driveDefaults())
  }

  def fireReaction: Bool = {
      triggers.map(_.resp.valid).reduce(_ || _) &&
        dependencies.map(_.resp.valid).foldLeft(true.B)(_ || _)  &&
        precedenceIn.map(_.resp.valid).foldLeft(true.B)(_ || _)
  }

  def hasPresentTriggers: Bool = {
    triggers.map(_.resp.present).reduce(_ || _)
  }
  // FIXME: This function should return a Bool which is true when it is done
  def reactionBody: Unit

  val reactionEnable = WireDefault(false.B)
  val reactionDone = WireDefault(false.B)
  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regState = RegInit(sIdle)
  val regCycles = RegInit(0.U(32.W))


  def reactionMain(): Unit = {
    require(triggers.length > 0, "[Reaction.scala] Reaction has no triggers")
    driveDefaults()

    switch(regState) {
      is(sIdle) {
        regCycles := 0.U

        when(fireReaction) {
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
}
