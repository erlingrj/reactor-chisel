package reactor

import chisel3._
import chisel3.util._
import reactor.Schedule._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

class TimerTriggerIO(nTimers: Int) extends Bundle {
  val timers = Vec(nTimers, new TriggerIO())
  def driveDefaults() = {
    timers.foreach(_.trigger.driveDefaults())
  }
}

class CoordinationIO extends Bundle {
  val nextEventTag = Output(Tag())
  val tagAdvanceGrant = Input(Tag())
  val logicalTagComplete = Output(Tag())
  val requestShutdown = Input(Bool()) // External request for termination at TAG

  def driveDefaults(): Unit = {
    nextEventTag := 0.U
    logicalTagComplete := Tag.FOREVER
  }

  def connectChild(c: CoordinationIO) = {
    nextEventTag := c.nextEventTag
    c.tagAdvanceGrant := tagAdvanceGrant
    logicalTagComplete := c.logicalTagComplete
    c.requestShutdown := requestShutdown
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
class MainClock extends Module {
  val io = IO(new MainClockIO())

  val regClock = RegInit(Tag(0))
  when(io.setTime.valid) {
    regClock := io.setTime.bits
  }.otherwise {
    regClock := regClock + 1.U
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

class TriggerGeneratorIO(nTimers: Int) extends Bundle {
  val triggers = new TimerTriggerIO(nTimers)
  val coordination = new CoordinationIO()
  val execute = Decoupled(new ExecuteIO())

  val inputPresent = Input(Bool())
  val terminate = Output(Bool())

  def driveDefaults() = {
    triggers.driveDefaults()
    coordination.driveDefaults()
    execute.valid := false.B
    execute.bits.driveDefaults()
    terminate := false.B
  }
}

class TriggerGenerator(timeout: Time, mainReactor: Reactor) extends Module {
  val nTimers = mainReactor.triggerIO.allTriggers.size
  val io = IO(new TriggerGeneratorIO(nTimers))
  io.driveDefaults()

  // The clock
  val mainClock = Module(new MainClock()).io
  mainClock.driveDefaultsFlipped()

  // Create the schedule and the event queue.
  val (hyperperiod, initialSchedule, periodicSchedule, shutdown) = createSchedules(mainReactor.allTriggerConfigs().map(_.cfg).toSeq)
  printSchedules((initialSchedule, periodicSchedule))
  val eventQueueParams = EventQueueParams(
    nTimers,
    hyperperiod,
    timeout,
    shutdown,
    initialSchedule,
    periodicSchedule)

  val eventQueue = Module(new EventQueue(eventQueueParams)).io
  eventQueue.driveDefaultsFlipped()
  io.terminate := eventQueue.terminate

  // The scheduler
  val scheduler = Module(new Scheduler()).io
  scheduler.tagAdvanceGrant := io.coordination.tagAdvanceGrant
  scheduler.now := mainClock.now
  scheduler.swInputPresent := io.inputPresent
  scheduler.nextEventTag := eventQueue.nextEventTag
  scheduler.execute <> io.execute


  // State machine for handling the firing of each event. We allow backpressure from the Reactors.
  // I.e. we will block in sFiring until all reactors are ready to receive the events.
  val sIdle :: sFiring :: Nil = Enum(2)
  val regState = RegInit(sIdle)
  val regTriggerFired = RegInit(VecInit(Seq.fill(nTimers)(false.B)))

  switch (regState) {
    is (sIdle) {
      when (scheduler.execute.valid) {
        regState := sFiring
      }
    }

    // We stay in this state until all the triggers have fired sucessfully. Essentially, we are allowing
    // backpressure. This implies that we might be backpressured for a long enough time so that we lose events.
    is (sFiring) {
      for (i <- 0 until io.triggers.timers.size) {
        when(!regTriggerFired(i)) {
          when(io.triggers.timers(i).trigger.ready) {
            when(EventMode.hasLocalEvent(scheduler.execute.bits.asUInt)) {
              io.triggers.timers(i).trigger.req.valid := true.B
              io.triggers.timers(i).trigger.req.present := eventQueue.triggerVec(i)
              io.triggers.timers(i).trigger.req.token.tag := eventQueue.nextEventTag
              io.triggers.timers(i).trigger.fire := true.B
            }.elsewhen(EventMode.hasExternalEvent(scheduler.execute.bits.asUInt)) {
              io.triggers.timers(i).trigger.writeAbsent()
            }
            regTriggerFired(i) := true.B
          }
        }
      }

      // Check that all events have been fired. If so, go back to accepting new events.
      when (regTriggerFired.asUInt.andR) {
        when(EventMode.hasLocalEvent(scheduler.execute.bits.asUInt)) {
         eventQueue.step := true.B
        }
        regState := sIdle
        regTriggerFired.foreach(_ := false.B)
        scheduler.execute.ready := true.B
      }
    }
  }
}
