package reactor

import chisel3._
import chisel3.util._
import reactor.Schedule._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

class ShutdownCommand extends Bundle {
  val valid = Bool() // Is the shutdown valid
  val independent = Bool() // Is the shutdown simultanous to an event from the FPGA?
}

class CoordinationIO extends Bundle {
  val nextEventTag = Output(Tag())
  val tagAdvanceGrant = Input(Tag())
  val logicalTagComplete = Output(Tag())
  val shutdownCommand = Input(new ShutdownCommand) // External request for termination at TAG

  def driveDefaults(): Unit = {
    nextEventTag := 0.U
    logicalTagComplete := Tag.FOREVER
  }
}

class MainClockIO extends Bundle {
  val setTime = Flipped(Valid(Tag()))
  val now = Output(Tag())

  def driveDefaultsFlipped() = {
    setTime.valid := false.B
    setTime.bits := Tag(0)
  }
}
class MainClock(implicit cfg: GlobalReactorConfig) extends Module {
  val io = IO(new MainClockIO())

  // We initialize the clock to the triggerLatecy. This accounts for the trigger latency be releasing
  // everything a bit early
  val regClock = RegInit(Tag(cfg.triggerLatency))
  when(io.setTime.valid) {
    regClock := io.setTime.bits + cfg.triggerLatency.U
  }.otherwise {
    regClock := regClock + cfg.clockPeriod.nanoseconds.U
  }
  io.now := regClock
}

/**
 * The TriggerGenerator is in charge of driving and reading tokens on the top-level ports of the main reactor.
 * It performs the control logic involved in coordinating the logical time of the hardware and the software
 * @param mainReactor
 */

class ExecuteIO extends Bundle {
  val eventMode = Output(EventMode())
  val tag = Output(Tag())

  def driveDefaults() = {
    eventMode := EventMode.noEvent
    tag := Tag(0)
  }
}

class TriggerGeneratorIO(nTimers: Int, nPhys: Int) extends Bundle {
  val timerTriggers = Vec(nTimers, new PureTokenWriteMaster())
  val phyTriggers = Vec(nPhys, new PureTokenWriteMaster())
  val nextEventTag = Output(Tag())
  val tagAdvanceGrant = Input(Tag())
  val shutdownCommand = Input(new ShutdownCommand) // External request for termination at TAG
  val coordinationValid = Input(Bool())
  val execute = Decoupled(new ExecuteIO())

  val inputPresent = Input(Bool())
  val terminate = Output(Bool())
  val phySchedules = Vec(nPhys, new PureTokenWriteSlave())

  def driveDefaults() = {
    timerTriggers.foreach(_.driveDefaults())
    execute.valid := false.B
    execute.bits.driveDefaults()
    terminate := false.B
    nextEventTag := Tag(0)
    phyTriggers.foreach(_.driveDefaults())
    phySchedules.foreach(_.driveDefaults())
  }
}

class TriggerGenerator(mainReactor: Reactor)(implicit val cfg: GlobalReactorConfig) extends Module {
  val nTimers = mainReactor.triggerIO.allTimerTriggers.size
  val nPhys = mainReactor.physicalIO.getAllPorts.size
  def nTriggers = nTimers + nPhys

  val io = IO(new TriggerGeneratorIO(nTimers, nPhys))
  io.driveDefaults()

  // The clock
  val mainClock = Module(new MainClock()).io
  mainClock.driveDefaultsFlipped()

  // Create the schedule and the event queue.
  val (hyperperiod, initialSchedule, periodicSchedule, shutdown) = createSchedules(mainReactor.allTriggerConfigs().map(_.cfg).toSeq)
  printSchedules((initialSchedule, periodicSchedule))
  val tokenQueueParams = TokenQueueParams(
    nTriggers,
    nTimers,
    nPhys,
    hyperperiod,
    cfg.timeout,
    shutdown,
    initialSchedule,
    periodicSchedule)

  val tokenQueue = Module(new TokenQueueMux(tokenQueueParams))
  tokenQueue.io.driveDefaultsFlipped()
  io.terminate := tokenQueue.io.terminate
  io.nextEventTag := tokenQueue.io.nextEventTag.bits
  tokenQueue.io.phySchedules zip io.phySchedules foreach {f => f._1 <> f._2}
  // Drive the tag signal here, from the clock. A physical action gets the current time as its tag.
  tokenQueue.io.phySchedules.foreach(_.tag := mainClock.now)

  // The scheduler
  val scheduler = Module(new Scheduler()).io
  scheduler.tagAdvanceGrant.bits := io.tagAdvanceGrant
  scheduler.tagAdvanceGrant.valid := io.coordinationValid
  scheduler.now := mainClock.now
  scheduler.swInputPresent := io.inputPresent
  scheduler.nextEventTag.valid := tokenQueue.io.nextEventTag.valid
  scheduler.nextEventTag.bits := tokenQueue.io.nextEventTag.bits
  scheduler.execute <> io.execute

  // The following is just for debug
  val regLastEventTag = RegInit(Tag.NEVER)
  when(scheduler.execute.fire) {
    assert(!(scheduler.execute.bits.tag === regLastEventTag && !io.shutdownCommand.valid), "[TriggerGenerator] Scheduler fired twice with the same tag and it was not a shutdown")
    regLastEventTag := scheduler.execute.bits.tag
  }

  // State machine for handling the firing of each event. We allow backpressure from the Reactors.
  // I.e. we will block in sFiring until all reactors are ready to receive the events.
  // FIXME: Optimize the case when we have zero local triggers.
  val sIdle :: sFire :: Nil = Enum(2)
  val regState = RegInit(sIdle)
  val regExecute = RegInit(EventMode.noEvent)
  val regTriggerFired = if (nTriggers > 0 )Some(RegInit(VecInit(Seq.fill(nTriggers)(false.B)))) else None

  switch (regState) {
    is (sIdle) {
      scheduler.execute.ready := true.B
      when (scheduler.execute.valid) {
        regState := sFire
        regExecute := scheduler.execute.bits.eventMode
      }
    }

    // We stay in this state until all the triggers have fired sucessfully. Essentially, we are allowing
    // backpressure. This implies that we might be backpressured for a long enough time so that we lose events.
    is (sFire) {
      scheduler.execute.ready := false.B
      // If we have local triggers on the FPGA, then we allow them to backpressure. If no local triggers. Then
      // we are essentially always in the Codesign situation. Then backpressure is implemented by the LTC signal.
      if (nTriggers > 0) {
        // Drive all output trigger signals:
        val triggers = io.timerTriggers ++ io.phyTriggers
        for ((t, i) <- triggers.zipWithIndex) {
          when(!regTriggerFired.get(i)) {
            when(t.req.ready) {
              when(EventMode.hasLocalEvent(regExecute.asUInt)) {
                t.fire := true.B
                t.req.valid := tokenQueue.io.triggerVec(i)
                t.absent := !tokenQueue.io.triggerVec(i)
                t.tag := tokenQueue.io.nextEventTag.bits
              }.elsewhen(EventMode.hasExternalEvent(regExecute.asUInt)) {
                t.writeAbsent()
              }
              regTriggerFired.get(i) := true.B
            }
          }
        }

        // Check that all events have been fired. If so, go back to accepting new events.
        when(regTriggerFired.get.asUInt.andR) {
          when(EventMode.hasLocalEvent(regExecute.asUInt)) {
            tokenQueue.io.nextEventTag.ready := true.B
          }
          regState := sIdle
          regTriggerFired.get.foreach(_ := false.B)
        }
      } else {
        regState := sIdle
      }
    }
  }
}
