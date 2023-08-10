package reactor

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer

/**
 * First some utility scala functions for calculating the hyperperiod and schedule based on
 * a set of Timers.
 */
case class ScheduleElement(tag: Time, triggers: Seq[Boolean])
object Schedule {
  type EventSchedule = Seq[ScheduleElement]

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  /**
   * Calculate the hyperperiod of the timers. We must consider both periods and offsets.
   * @param configs
   * @return
   */
  def calculateHyperPeriod(configs: Seq[TimerConfig]): Time = {
    Time.nsec(
      configs
        .filterNot(_.period.ticks == 0)
        .map(_.period.ticks)
        .reduceOption(lcm).getOrElse(0)
    )
  }

  // Calculate the steps present in the schedule.
  def calculateScheduleSteps(hyperPeriod: Time, configs: Seq[TimerConfig]): Seq[Time] = {
    val steps = ArrayBuffer[Time]()
    for (c <- configs) {
      if (!c.isShutdownTrigger) {
        var time = c.offset
        while (time == 0 || time < hyperPeriod) {
          if (!steps.contains(time)) steps += time
          if (c.period > 0) time += c.period else time = Time.FOREVER
        }
      }
    }
    steps.sorted.toSeq
  }

  def printSchedules(scheds: (EventSchedule, EventSchedule)): Unit = {
    println("**** Initial Schedule: ")
    for (el <- scheds._1.zipWithIndex) {
      println(s"${el._2}. Tag=${el._1.tag} TriggerVec=${el._1.triggers}")
    }

    println("**** Periodic Schedule: ")
    for (el <- scheds._2.zipWithIndex) {
      println(s"${el._2}. Tag=${el._1.tag} TriggerVec=${el._1.triggers}")
    }
  }

  def createSchedules(configs: Seq[TimerConfig]): (Time, EventSchedule, EventSchedule, ScheduleElement)= {
    val hyperPeriod = calculateHyperPeriod(configs)
    val periodicSchedule = ArrayBuffer[ScheduleElement]()
    val initialSchedule = ArrayBuffer[ScheduleElement]()
    val steps = calculateScheduleSteps(hyperPeriod, configs)
    println(s"Hyper period is: ${hyperPeriod.ticks} with steps: `$steps`")

    for (i <- steps) {
      // The periodic schedule is all timers with periods greater than zero
      println(i.nsec - configs(0).offset.nsec)
      println(configs(0).period.nsec)

      val periodic = configs.map(c =>
        c.period > 0 &&
        !c.isShutdownTrigger &&
        ((i.nsec - c.offset.nsec) % c.period.nsec == 0)
      )

      // Initial schedule is just all timers
      val initial = configs.map(c =>
        (i.nsec == c.offset.nsec) &&
          !c.isShutdownTrigger
      )

      periodicSchedule += ScheduleElement(i, periodic)
      initialSchedule += ScheduleElement(i, initial)
    }
    val shutdownTriggers = configs.map(_.isShutdownTrigger)
    val shutdown = ScheduleElement(Time.FOREVER, shutdownTriggers)

    (hyperPeriod, initialSchedule.toSeq, periodicSchedule.toSeq, shutdown)
  }
}

/**
 * The parameters needed to build an Event Queue
 * @param nLocalTriggers
 * @param hyperPeriod
 * @param shutdownTime
 * @param shutdownTriggers
 * @param initialSchedule
 * @param periodicSchedule
 */
case class EventQueueParams(
    nLocalTriggers: Int,
    hyperPeriod: Time,
    shutdownTime: Time,
    shutdownTriggers: ScheduleElement,
    initialSchedule: Schedule.EventSchedule,
    periodicSchedule: Schedule.EventSchedule
) {
  def scheduleLength = initialSchedule.size
  def scheduleLengthBits = if (scheduleLength > 0) Math.max(log2Ceil(scheduleLength), 1) else 0
  def scheduleWidth = nLocalTriggers
  def scheduleWidthBits = Math.max(log2Ceil(scheduleWidth), 1)

  require(nLocalTriggers == shutdownTriggers.triggers.size)
}

class EventQueueIO(p: EventQueueParams) extends Bundle {
  val nextEventTag = Output(Tag())
  val triggerVec = Vec(p.scheduleWidth, Output(Bool()))
  val step = Input(Bool())
  val terminate = Output(Bool())

  def driveDefaultsFlipped(): Unit = {
    step := false.B
  }
  def driveDefaults(): Unit  = {
    nextEventTag := Tag.NEVER
    terminate := false.B
    triggerVec.map(_ := false.B)
  }
}

/**
 * Has two schedules. One for the very first hyperperiod, and one for the rest.
 * This puts a limitation on that the offset cannot be greater than the period. This is too limiting and should be
 * fixed later. We also handle the shutdown.
 * initialSchedule and periodicSchedule must contain the VERY SAME steps. Which means that we can have some totally absent
 * steps in the periodicSchedule. This should be fixed. An edge case that complicates things is when we only have initial events
 * and no periodic. E.g. only startup and shutdown events.
 * @param p
 */
// FIXME: There are more efficient ways of handling the initial/periodic schedule. Also consider when they are identical
class EventQueue(p: EventQueueParams) extends Module {
  val io = IO(new EventQueueIO(p))
  io.driveDefaults()

  val nextLocalEvent = WireDefault(Tag.NEVER)
  io.nextEventTag := nextLocalEvent

  // Only bother with the event queue if we actually have locally originating events.
  if (p.scheduleLength > 0) {
    val initialRound = RegInit(true.B)

    val idx = RegInit(0.U(p.scheduleLengthBits.W))
    val triggerVecInital = RegInit(VecInit(Seq.tabulate(p.initialSchedule.length) { i =>
      VecInit(Seq.tabulate(p.initialSchedule(0).triggers.size) { j =>
        p.initialSchedule(i).triggers(j).B
      })
    }))

    val triggerVecPeriodic = RegInit(VecInit(Seq.tabulate(p.initialSchedule.length) { i =>
      VecInit(Seq.tabulate(p.periodicSchedule(0).triggers.size) { j =>
        p.periodicSchedule(i).triggers(j).B
      })
    }))

    val tags = RegInit(VecInit(Seq.tabulate(p.initialSchedule.size) { i => Tag(p.initialSchedule(i).tag) }))
    val epoch = RegInit(Tag(0))
    nextLocalEvent := tags(idx) + epoch

    // Do first the initial round. Then the periodic (if we have one)
    when(initialRound) {
      for ((triggerIO, trigger) <- io.triggerVec zip triggerVecInital(idx)) {
        triggerIO := trigger
      }
    }.otherwise {
      if (p.hyperPeriod > 0) {
        for ((triggerIO, trigger) <- io.triggerVec zip triggerVecPeriodic(idx)) {
          triggerIO := trigger
        }
      } else {
        nextLocalEvent := Tag.FOREVER
      }
    }

    when(io.step) {
      idx := idx + 1.U
      when(idx === (p.scheduleLength - 1).U) {
        // After first wrap, we start using the periodic schedule
        initialRound := false.B
        idx := 0.U
        epoch := epoch + p.hyperPeriod.ticks.U
      }
    }
  } else {
    println("Empty schedule. Only compiling a shell for the EventQueue")
  }

  if (p.shutdownTime != Time.NEVER) {
    val regDone = RegInit(false.B)

    when(nextLocalEvent > p.shutdownTime.ticks.U) {
      io.nextEventTag := p.shutdownTime.ticks.U

      for ((triggerIO, trigger) <- io.triggerVec zip p.shutdownTriggers.triggers) {
        triggerIO := trigger.B
      }

      when (io.step) {
        regDone := true.B
      }
    }

    when (regDone) {
      io.nextEventTag := Tag.NEVER
      io.terminate := true.B
    }
    assert(!(io.step && regDone))
  }
}
