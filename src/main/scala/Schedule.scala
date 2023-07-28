package reactor

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer


case class ScheduleElement(tag: Time, triggers: Seq[Boolean])
type Schedule = Seq[ScheduleElement]

def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)
def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

def calculateHyperPeriod(configs: Seq[TimerConfig]): Time = {
  Time.nsec(configs.map(_.period.nsec).reduce(lcm))
}

def createSchedule(configs: Seq[TimerConfig]): Schedule = {
  val hyperPeriod = calculateHyperPeriod(configs)
  val schedule = ArrayBuffer[ScheduleElement]()

  for (i <- 0L until hyperPeriod.nsec) {
    val booleans = configs.map(config => (i - config.offset.nsec) % config.period.nsec == 0)
    schedule += ScheduleElement(Time.nsec(i), booleans)
  }

  schedule.toSeq
}


/**
 *
 * @param triggerVecs A matrix of booleans showing who
 * @param tags
 */
case class EventQueueParams(
    schedule: Schedule
) {
  require(schedule.size > 0, "No support for purely reactive FPGA reactors")
  def scheduleLength = schedule.size
  def scheduleLengthBits = Math.max(log2Ceil(scheduleLength), 1)
  def scheduleWidthBits = Math.max(log2Ceil(schedule(0).triggers.size), 1)
}

class EventQueueIO(p: EventQueueParams) extends Bundle {
  val nextEventTag = Output(new Tag())
  val triggerVec = Vec(p.scheduleWidthBits, Output(Bool()))
  val step = Input(Bool())
}

class EventQueue(p: EventQueueParams) extends Module {
  val io = IO(new EventQueueIO(p))

  val idx = RegInit(0.U(p.scheduleLengthBits.W))
  val triggerVec = RegInit(VecInit(Seq.tabulate(p.schedule.length) { i =>
    VecInit(Seq.tabulate(p.schedule(0).triggers.size) { j =>
      p.schedule(i).triggers(j).B
    })}))

  val tags = RegInit(VecInit(Seq.tabulate(p.schedule.size) { i => Tag(p.schedule(i).tag }))

  io.nextEventTag := tags(idx)
  for ((triggerIO, trigger) <- io.triggerVec zip triggerVec(idx)) {
    triggerIO := trigger
  }

  when (io.step) {
    idx := idx + 1.U
    when (idx === (p.scheduleLength - 1).U) {
      idx := 0.U
    }
  }
}


