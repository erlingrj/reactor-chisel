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
  val nextEventTag = Output(Tag())
  val tagAdvanceGrant = Input(Tag())
  val shutdownCommand = Input(new ShutdownCommand) // External request for termination at TAG
  val coordinationValid = Input(Bool())
  val execute = Decoupled(new ExecuteIO())

  val inputPresent = Input(Bool())
  val terminate = Output(Bool())

  def driveDefaults() = {
    triggers.driveDefaults()
    execute.valid := false.B
    execute.bits.driveDefaults()
    terminate := false.B
    nextEventTag := Tag(0)
  }
}

class TriggerGenerator(standalone: Boolean, timeout: Time, mainReactor: Reactor) extends Module {
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

  val eventQueue = if (standalone) Module(new EventQueueStandalone(eventQueueParams)) else Module(new EventQueueCodesign(eventQueueParams))

  eventQueue.io.driveDefaultsFlipped()
  io.terminate := eventQueue.io.terminate
  io.nextEventTag := eventQueue.io.nextEventTag

  if(!standalone) {
    val eq = eventQueue.asInstanceOf[EventQueueCodesign]
    eq.shutdownIO.simultanous := io.shutdownCommand.valid && !io.shutdownCommand.independent
    eq.shutdownIO.independent := io.shutdownCommand.valid && io.shutdownCommand.independent
    eq.shutdownIO.independentTag := io.tagAdvanceGrant
  }


  // The scheduler
  val scheduler = Module(new Scheduler()).io
  scheduler.tagAdvanceGrant.bits := io.tagAdvanceGrant
  scheduler.tagAdvanceGrant.valid := io.coordinationValid
  scheduler.now := mainClock.now
  scheduler.swInputPresent := io.inputPresent
  scheduler.nextEventTag := eventQueue.io.nextEventTag
  scheduler.execute <> io.execute

  // The following is just for debug
  val regLastEventTag = RegInit(Tag.NEVER)
  when(scheduler.execute.fire) {
    assert(!(scheduler.execute.bits.tag === regLastEventTag && !io.shutdownCommand.valid), "[TriggerGenerator] Scheduler fired twice with the same tag and it was not a shutdown")
    regLastEventTag := scheduler.execute.bits.tag
  }

  // State machine for handling the firing of each event. We allow backpressure from the Reactors.
  // I.e. we will block in sFiring until all reactors are ready to receive the events.
  val sIdle :: sFiring :: Nil = Enum(2)
  val regState = RegInit(sIdle)
  val regExecute = RegInit(EventMode.noEvent)
  val regTriggerFired = RegInit(VecInit(Seq.fill(nTimers)(false.B)))

  switch (regState) {
    is (sIdle) {
      scheduler.execute.ready := true.B
      when (scheduler.execute.valid) {
        regState := sFiring
        regExecute := scheduler.execute.bits.eventMode
      }
    }

    // We stay in this state until all the triggers have fired sucessfully. Essentially, we are allowing
    // backpressure. This implies that we might be backpressured for a long enough time so that we lose events.
    is (sFiring) {
      scheduler.execute.ready := false.B
      for (i <- 0 until io.triggers.timers.size) {
        when(!regTriggerFired(i)) {
          when(io.triggers.timers(i).trigger.ready) {
            when(EventMode.hasLocalEvent(regExecute.asUInt)) {
              io.triggers.timers(i).trigger.req.valid := true.B
              io.triggers.timers(i).trigger.req.present := eventQueue.io.triggerVec(i)
              io.triggers.timers(i).trigger.req.token.tag := eventQueue.io.nextEventTag
              io.triggers.timers(i).trigger.fire := true.B
            }.elsewhen(EventMode.hasExternalEvent(regExecute.asUInt)) {
              io.triggers.timers(i).trigger.writeAbsent()
            }
            regTriggerFired(i) := true.B
          }
        }
      }

      // Check that all events have been fired. If so, go back to accepting new events.
      when (regTriggerFired.asUInt.andR) {
        when(EventMode.hasLocalEvent(regExecute.asUInt)) {
         eventQueue.io.step := true.B
        }
        regState := sIdle
        regTriggerFired.foreach(_ := false.B)
      }
    }
  }
}
