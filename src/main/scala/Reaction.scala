package reactor

import chisel3._
import chisel3.util._


abstract class ReactionIO (implicit rc: ReactorGlobalParams) extends Bundle {
  // Control signals
  val done = Output(Bool())
  val running = Output(Bool())
  val enable = Flipped(Decoupled())

  def triggers: Vec[PortOutIO[_<:Data]]
  def dependencies: Vec[PortOutIO[_<:Data]]
  def antiDependencies: Vec[PortInIO[_<:Data]]

  def tieOff: Unit = {
    done := false.B
    running := false.B
    enable.ready := false.B
  }
}


abstract class Reaction(implicit rc: ReactorGlobalParams) extends Module {
  val io: ReactionIO
  io.tieOff

  def reaction: Bool

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  val regPresent = if (rc.devel) Some(RegInit(VecInit(Seq.fill(io.triggers.length)(false.B)))) else None

  switch(regState) {

    is(sIdle) {
      io.running := false.B
      io.done := false.B

      when(io.enable.valid) {
        when(io.triggers.map(_.present).reduce(_ || _)) {
          (regPresent.get zip io.triggers).map(f => f._1 := f._2.present)
          regState := sRunning
          io.enable.ready := true.B
        }
      }
    }

    is(sRunning) {
      io.running := true.B
      io.done := false.B

      val done = reaction

      when(done) {
        regState := sDone
      }

      // Verify that we don't get new triggers WHILE we are executing. SHould start execution at consistent logical time
      //  new triggers should not arrive after this
      if (rc.devel) {
        (regPresent.get zip io.triggers).map(f => assert(f._1 === f._2.present, "[Reaction.scala] A new trigger signal appeared after execution of Reaction had started"))
      }

    }

    is(sDone) {
      io.done := true.B
      io.running := false.B
      regState := sIdle
      if (rc.devel) regPresent.get.map(_ := false.B)
    }
  }
}
