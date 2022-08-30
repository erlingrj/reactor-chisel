package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.VecLiterals._
import chisel3.experimental.{DataMirror, Direction}

// An implementation of the Synchronous MoC Scheduler

case class SchedulerConfig(
  schedule: Seq[Seq[Int]] = Seq(Seq(0)),
) {

  def schedLen = schedule.length
  def nReactions: Int = schedule.flatten.max + 1

  def schedLenBits: Int = {
    if (schedLen == 1) 1
    else log2Ceil(schedLen)
  }
  def reactionBits: Int = log2Ceil(nReactions)

  def schedBits: Seq[Int] = {
    Seq.tabulate(schedLen)(i =>
      {
        var sched = 0
        for (j <- 0 until nReactions) {
          if (schedule(i).contains(j)) {
            sched = sched | (1 << j)
          }
        }
        sched
      })
    }

  def checkConfig() = {

  }

  require(schedule.length > 0)
  for (i <- 0 until nReactions) {
    val n = schedule.flatten.count(_ == i)
    require(n == 1, s"[Scheduler.scala] Illegal schedule contains $n occurrences of reaction $i")
  }

  println(s"Schedule length=$schedLen\nSchedule bits=$schedLenBits\nScheduleBits=$schedBits")
}

class SchedulerCtrlIO extends Bundle {
  val start = Flipped(Decoupled())
  val running = Output(Bool())
  val done = Output(Bool())

  def tieOff = {
      start.ready := false.B
      running := false.B
      done := false.B
    }
  def tieOffExt = {
    start.valid := false.B
  }

}

class Scheduler(val c: SchedulerConfig) extends Module {
  val ioSchedulerCtrl = IO(new SchedulerCtrlIO())
  val ioReactionCtrl = IO(Vec(c.nReactions, Flipped(new ReactionCtrlIO())))

  ioSchedulerCtrl.tieOff
  ioReactionCtrl.map(_.tieOff)

  // Extract schedule from config. Turn it into a more convenient rep. E.g.
  // c.schedule = ( (1,3), (2,4) )
  // schedule  = ( (1,0,1,0), (0,1,0,1) )
  val schedule = VecInit(c.schedBits.map(_.U))

  // Idx for counting each step of the schedule
  val regSchedStepIdx = RegInit(0.U(c.schedLenBits.W))

  // Keeping track of which Reactions are done in each step of the schedule
  val regReactionsRunning = RegInit(VecInit(Seq.fill(c.nReactions)(false.B)))

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regStateMain = RegInit(sIdle)
  val regStateSchedStep = RegInit(sIdle)

  switch(regStateMain) {
    is (sIdle) {
      ioSchedulerCtrl.start.ready := true.B
      when (ioSchedulerCtrl.start.fire) {
        regStateMain := sRunning
        regReactionsRunning.map(_ := false.B)
      }
    }
    is (sRunning) {
      ioSchedulerCtrl.running := true.B

      switch(regStateSchedStep) {
        is (sIdle) {
          // Start the reactions on the current step
          val currSched = schedule(regSchedStepIdx)
          for (i <- 0 until c.nReactions) {
            ioReactionCtrl(i).enable.valid := currSched(i)
            assert(!(currSched(i) && !ioReactionCtrl(i).enable.fire), "[Scheduler.scala] Reaction was enabled but did not fire")

            regReactionsRunning(i) := currSched(i)
          }
          regStateSchedStep := sRunning
        }

        is (sRunning) {
          // Check if any reaction finished this step
          for (i <- 0 until c.nReactions) {
            when (ioReactionCtrl(i).done) {
              assert (regReactionsRunning(i), "[Scheduler.scala] reaction done, but was not marked as running")
              regReactionsRunning(i) := false.B
            }
          }

          // Check if all reactions are done
          when (!regReactionsRunning.reduce(_||_)) {
            regStateSchedStep := sDone
          }
        }

        is (sDone) {
          // Progress to the next step
          regStateSchedStep := sIdle
          when (regSchedStepIdx === (c.schedLen-1).U) {
            regStateMain := sDone
          } otherwise {
            regSchedStepIdx := regSchedStepIdx + 1.U
          }
        }
      }
    }

    is (sDone) {
      ioSchedulerCtrl.running := true.B
      ioSchedulerCtrl.done := true.B
      regStateMain := sIdle
      regSchedStepIdx := 0.U
    }
  }
}
