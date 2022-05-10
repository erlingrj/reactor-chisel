package reactor

import chisel3._
import chisel3.util._
import scala.collection.mutable.ListBuffer


abstract class AbstractScheduler extends Module

class SchedulerIO(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Bundle {


  val execute = Flipped(Decoupled())
  val arbiterEn = Decoupled()
  val reactorsEn = Vec(c.reactors.length, Decoupled())
  val clockKeeperStart = Decoupled()


  def tieOff(): Unit = {
    execute.ready := false.B
    arbiterEn.valid := false.B
    reactorsEn.map(_.valid := false.B)
    clockKeeperStart.valid := false.B
  }
}

/**
 *
 * Scheduler is in charge of enabling the arbiter and also contained Reactors
 * It is linked to the ClockKeeper but for now separated in different Modules
 *  IO:
 *    - execute: Input from superReactor to trigger the execution in this reactor. This happens after
 *    - arbiterEn: Output to the Arbiter which tells it to start enabling the Reactions in the Reactor
 *    - reactorsEn: Output to each contained Reactor (this goes to that Reactor's scheduler)
 *    - clockKeeperStart: Output to the ClockKeeper in this Reactor to inform it that it can start sorting
 *
 */
class Scheduler(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Module {
  val io = IO(new SchedulerIO(c))
  io.tieOff()

  val levels = c.getNumReactorLevels()

  val execSchedule = if(levels > 0) Some(Module(new ExecutionSchedule(c))) else None

  val sIdle :: sExec :: sBlock :: sSync :: sWaitForSched :: Nil = Enum(5)
  val regStateArb = RegInit(sIdle)
  val regStateReactors = RegInit(sIdle)
  io.execute.ready := (regStateArb === sIdle) && (regStateReactors === sIdle)

  switch(regStateArb) {
    is (sIdle) {
      when (io.execute.fire) {
        regStateArb := sExec
      }
    }

    is (sExec) {
      io.arbiterEn.valid := true.B
      when(io.arbiterEn.fire) {
          regStateArb := sBlock
        }
      }
    is (sBlock) {
      when (io.arbiterEn.ready) {
        // If we have sub-reactors go to sync state to sync with the sub-reactor execution
        if (levels > 0) {
          regStateArb := sSync
        } else {
          regStateArb := sWaitForSched
        }
      }
    }

    is (sSync) {
      when (regStateReactors === sSync) {
        regStateArb := sWaitForSched
      }
    }

    is (sWaitForSched) {
      io.clockKeeperStart.valid := true.B
      when (io.clockKeeperStart.fire) {
        regStateArb := sIdle
      }
    }
  }

  if (levels > 0) {
    val sched = execSchedule.get.io
    sched.tieOffExt()

    val regSchedule = RegInit(VecInit(Seq.fill(c.reactors.length)(false.B)))
    val regScheduleLast = RegInit(false.B)


    switch(regStateReactors) {
      is (sIdle) {
        when (io.execute.fire) {
          regStateReactors := sExec
        }
      }

      is (sExec) {
        sched.enabled.ready := true.B
        when(sched.enabled.fire) {
          for (i <- 0 until c.reactors.length) {
            when (sched.enabled.bits(i)) {
              io.reactorsEn(i).valid := true.B
              assert(io.reactorsEn(i).fire, s"[Scheduler.scala] Sub-reactor $i enabled but not ready")
            }
          }
          assert(sched.enabled.bits.reduce(_||_), "[Scheduler.scala] Scheduler dequeued empty schedule")

          regSchedule zip sched.enabled.bits map {f => f._1 := f._2}
          regScheduleLast := sched.last

          regStateReactors := sBlock
        }
      }

      is (sBlock) {
        for (i <- 0 until c.reactors.length) {
          when (regSchedule(i) && io.reactorsEn(i).ready) {
            regSchedule(i) := false.B
          }
        }

        when (regSchedule.reduce(_||_) === false.B) {
          when (regScheduleLast) {
            regStateReactors := sSync
          }.otherwise {
            regStateReactors := sExec
          }
        }
      }

      is (sSync) {
        when (regStateArb === sSync) {
          regStateReactors := sWaitForSched
        }
      }

      is (sWaitForSched) {
        when (io.clockKeeperStart.fire) {
          regStateReactors := sIdle
        }
      }
    }

  }
}

class ExecutionScheduleIO(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Bundle {
  val enabled = Decoupled(Vec(c.numReactions, Bool()))
  val last = Output(Bool())

  def tieOff(): Unit = {
    enabled.valid := false.B
    enabled.bits.map(_:=false.B)
    last := false.B
  }

  def tieOffExt(): Unit = {
    enabled.ready := false.B
  }
}

class ExecutionSchedule(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Module {
  // Find number of levels:
  val levels = c.getNumReactorLevels()
  val _reactionLevels = ListBuffer[Seq[Int]]()
  require(levels > 0, "[Secheduler.scala] ExecSchedule instantiated but there are no sub-reactors")

  println(s"ExecutionSchedule with ${levels} levels")
  for (l <- 0 until levels) {
    _reactionLevels.append(c.getReactorIndicesOnLevel(l))
  }
  val reactionLevels = _reactionLevels.toSeq
  println(s"ExecutionSchedule for Reactor `${c.id}` is $reactionLevels")

  val io = IO(new ExecutionScheduleIO(c))
  io.tieOff()

  val regSchedule = RegInit(
    VecInit(
      Seq.tabulate(levels)(i => VecInit(
        Seq.tabulate(c.numReactions)(j => reactionLevels(i).contains(j).B)
      ))
    )
  )

  val regCount = RegInit(0.U(log2Ceil(levels).W))

  io.last := regCount === (levels-1).U
  io.enabled.valid := true.B
  (io.enabled.bits zip regSchedule(regCount)).map(f => f._1 := f._2)

  when (io.enabled.fire) {
    regCount := regCount + 1.U
    when (regCount === (levels-1).U) {
      regCount := 0.U
    }
  }
}