package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror.directionOf



class ReactionCtrlIO (implicit rc: ReactorGlobalParams) extends Bundle {
  // Control signals
  val done = Output(Bool())
  val running = Output(Bool())
  val enable = Flipped(Decoupled())

  def tieOff: Unit = {
    if (directionOf(done) == ActualDirection.Output) {
      done := false.B
      running := false.B
      enable.ready := false.B
    } else
    {
      enable.valid := false.B
    }
  }
}

abstract class Reaction(implicit rc: ReactorGlobalParams) extends Module {
  val ioCtrl = IO(new ReactionCtrlIO())
  ioCtrl.tieOff

  val triggers: Seq[PortOutIO[UInt]]
  val dependencies: Seq[PortOutIO[UInt]]
  val antiDependencies: Seq[PortInIO[UInt]]

  // Reset signal to reset all Registers in the reactionBody
  val reactionEnable = Wire(Bool())
  reactionEnable := false.B
  val reactionDone = Wire(Bool())
  reactionDone := false.B

  def reactionBody: Unit

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  def reactionPrelude: Unit = {
    triggers.map(_.reactionTieOff)
    dependencies.map(_.reactionTieOff)
    antiDependencies.map(_.reactionTieOff)
  }

  // The reactionMain is the "mainLoop" of the Reaction. To avoid some quirks in Chisel'
  //  this is wrapped in a function and called from the child class
  def reactionMain: Unit = {
    switch(regState) {

      is(sIdle) {
        ioCtrl.running := false.B
        ioCtrl.done := false.B

        when(ioCtrl.enable.valid) {
          regState := sRunning
          ioCtrl.enable.ready := true.B
        }
      }

      is(sRunning) {
        ioCtrl.running := true.B
        ioCtrl.done := false.B
        reactionEnable := true.B

        withReset(!reactionEnable) {
          reactionBody
        }

        when(reactionDone) {
          regState := sDone
        }
      }

      is(sDone) {
        ioCtrl.done := true.B
        ioCtrl.running := true.B
        regState := sIdle
      }
    }
  }
}