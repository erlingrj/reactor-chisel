package reactor

import chisel3._
import chisel3.util._


class ReactionEnableIO()(implicit rc: ReactorGlobalParams) extends Bundle {
  val enable = Input(Bool())
  val fire = Output(Bool())
  val done = Output(Bool())
}


class ReactionIO(c: ReactionConfig)(implicit rc: ReactorGlobalParams) extends Bundle {
  // Control signals
  val done = Output(Bool())
  val running = Output(Bool())

  val arbiterEn = new ReactionEnableIO()

  // Input ports
  val dependencies = MixedVec(for (inPort <- c.dependencies) yield Flipped(Decoupled(TaggedSignal(inPort.gen))))
  val antiDependencies = MixedVec(for (inPort <- c.antiDependencies) yield Decoupled(TaggedSignal(inPort.gen)))
  val triggers = MixedVec(for (inPort <- c.triggers) yield Flipped(Decoupled(TaggedSignal(inPort.gen))))
  val schedules = MixedVec(for (inPort <- c.schedules) yield Decoupled(new TaggedSignal(inPort.gen)))


  // Return the port
  def get(name: String): DecoupledIO[TaggedSignal[_<:Data]] = {
    for ((c, i) <- c.dependencies.zipWithIndex) {
      if (c.id == name) return dependencies(i)
    }

    for ((c, i) <- c.antiDependencies.zipWithIndex) {
      if (c.id == name) return antiDependencies(i)
    }

    for ((c, i) <- c.triggers.zipWithIndex) {
      if (c.id == name) return triggers(i)
    }

    for ((c, i) <- c.schedules.zipWithIndex) {
      if (c.id == name) return schedules(i)
    }

    // If function has not returned yet, throw assertion
    assert(false.B, s"[Reaction.scala] ReactionIO.get did not find port with name= $name")
    WireInit(Decoupled(TaggedSignal(UInt(1.W), 0.U, 0.U)))
  }

  def tieOff: Unit = {
    done := false.B
    running := false.B
    antiDependencies.map(_.valid := false.B)
    antiDependencies.map(_.bits := 0.U)

    arbiterEn.done := false.B
    arbiterEn.fire := false.B
  }
}


abstract class Reaction(c: ReactionConfig)(implicit rc: ReactorGlobalParams) extends ReactorElement {
  val io = IO(new ReactionIO(c))
  io.tieOff

  def reaction: Bool

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  val regPresent = RegInit(VecInit(Seq.fill(c.triggers.length)(false.B)))


  switch(regState) {

    is(sIdle) {
      io.running := false.B
      io.done := false.B

      when (io.arbiterEn.enable) {
        when(io.triggers.map(_.valid).reduce(_ || _)) {
          (regPresent zip io.triggers).map(f => f._1 := f._2.valid)
          regState := sRunning
          io.arbiterEn.fire := true.B
        }
      }
    }

    is(sRunning) {
      io.running := true.B
      io.done := false.B
      io.arbiterEn.done := true.B

      val done =  reaction

      when(done) {
        regState := sDone
      }

      // Verify that we don't get new triggers WHILE we are executing. SHould start execution at consistent logical time
      //  new triggers should not arrive after this
      (regPresent zip io.triggers).map(f => assert(f._1 === f._2.valid, "[Reaction.scala] A new trigger signal appeared after execution of Reaction had started"))
    }

    is(sDone) {
      io.done := true.B

      // Fire trigger transaction
      for (i <- 0 until io.triggers.length) {
        when (regPresent(i)) {
          io.triggers(i).ready := true.B
        }
      }

      io.running := false.B
      regState := sIdle
      regPresent.map(_:=false.B)
    }
  }
}
