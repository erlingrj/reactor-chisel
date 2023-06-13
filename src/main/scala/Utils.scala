package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

import Numeric._
import scala.collection.mutable.ArrayBuffer

object ReactorUtils {
  def findHyperPeriod(periods: Seq[Int]): Int = {
    periods.foldLeft(1) { (lcm, n) =>
      lcm * n / BigInt(lcm).gcd(n).toInt
    }
  }

  // Given a timer config and a hyper period. Generate a schedule for this timer
  // The schedule is given as relative values, not absolute. E.g.
  // period=20 offset=5 hyper-period=60 gives the schedule
  // <5, 20, 20> I.e. wait 5 ticks, then wait 20 ticks then wait 20 ticks. Then wait for next hyper period
  def generateSchedule(t: TimerConfig, hyperPeriod: Time): Seq[Time] = {
    require(t.period > 0) // FIXME: Relax this requirement
    require(hyperPeriod > 0)
    require(t.period % hyperPeriod == 0)
    require(t.offset < t.period) // FIXME: Would be great to relax this constraint also

    val sched: ArrayBuffer[Time] = ArrayBuffer()
    var running = Time.nsec(0)

    // First schedule is the offset
    sched += t.offset
    running += t.period

    while (running < hyperPeriod) {
      sched += t.period
      running += t.period
    }
    sched.toSeq
  }

}

object ReactorFault extends ChiselEnum {
  val None, ReactionTimeout, MemoryFault = Value
}
