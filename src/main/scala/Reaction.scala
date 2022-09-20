package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror.directionOf



case class ReactionConfig(
  triggers : Array[PortIOConfig[Data]],
  antiDependencies : Array[PortIOConfig[Data]],
  dependencies : Array[PortIOConfig[Data]],
  states : Array[ReactorStateConfig[Data]]
)


class ReactionCtrlIO extends Bundle {
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

class ReactionPortIO(c: ReactionConfig) extends Bundle {
  val triggers = MixedVec(Seq.tabulate(c.triggers.length)(i => Flipped(new PortOutIO(c.triggers(i)))))
  val dependencies = MixedVec(Seq.tabulate(c.dependencies.length)(i => Flipped(new PortOutIO(c.dependencies(i)))))
  val antiDependencies = MixedVec(Seq.tabulate(c.antiDependencies.length)(i => Flipped(new PortInIO(c.antiDependencies(i)))))
}

class ReactionStateIO(c: ReactionConfig) extends Bundle {
  val states = MixedVec(Seq.tabulate(c.states.length)(i => Flipped(new ReactorStateIO(c.states(i)))))
}

abstract class Reaction(c: ReactionConfig) extends Module {
  val ioCtrl = IO(new ReactionCtrlIO())
  ioCtrl.tieOff
  val io = IO(new ReactionPortIO(c))
  io.triggers.foreach(_.reactionTieOff)
  io.dependencies.foreach(_.reactionTieOff)
  io.antiDependencies.foreach(_.reactionTieOff)

  val ioState = IO(new ReactionStateIO(c))
  ioState.states.foreach(_.tieOffExt)


  // Reset signal to reset all Registers in the reactionBody
  val reactionEnable = Wire(Bool())
  reactionEnable := false.B
  val reactionDone = Wire(Bool())
  reactionDone := false.B

  // TODO: reactionBody should return Bool saying whether it is done or not
  def reactionBody: Unit

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regStateTop = RegInit(sIdle)
  val regCycles = RegInit(0.U(16.W))


  // The reactionMain is the "mainLoop" of the Reaction. To avoid some quirks in Chisel'
  //  this is wrapped in a function and called from the child class
  def reactionMain: Unit = {
    switch(regStateTop) {

      is(sIdle) {
        ioCtrl.running := false.B
        ioCtrl.done := false.B
        regCycles := 0.U

        when(ioCtrl.enable.valid) {
          ioCtrl.enable.ready := true.B
          // If there is data present at any port go to running.
          when(io.triggers.map(_.present).reduce(_||_)) {
            regStateTop := sRunning
          } otherwise {
            // If there is nothing at the trigger ports, then
            //  go directly done
            regStateTop := sDone
          }

        }
      }

      is(sRunning) {
        regCycles := regCycles + 1.U
        ioCtrl.running := true.B
        ioCtrl.done := false.B
        reactionEnable := true.B

        withReset(!reactionEnable) {
          reactionBody
        }

        when(reactionDone) {
          regStateTop := sDone
        }
      }

      is(sDone) {
        ioCtrl.done := true.B
        ioCtrl.running := true.B
        regStateTop := sIdle
      }
    }
  }

  assert(!(regCycles > 200.U), "[Reaction] Reaction was running for over 200cc assumed error")
}

