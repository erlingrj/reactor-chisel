package reactor

import chisel3._
import chisel3.util._

case class TimerConfig(
                      offset: Int,
                      period: Int
                      ) {
  def ==(other: TimerConfig): Boolean = {
    other.period == period && other.offset == offset
  }
}
class TimerIO extends Bundle {
  val trigger = new EventWriteMaster(UInt(0.W), new PureToken())
}

/**
 * Due to our Dataflow semantics, the Timer needs to know about the <period, offset> of all other timers in the program
 * also. This is so that it can generate absent tokens at the trigger instants of those timers. Unless it has its own
 * trigger scheduled for this instant.
 *
 * FIXME: We might require some sort of buffering inside the connection in order to support e.g. timers with 1 tick
 *  unalignment. That will require all timers to generate events on both those ticks. And an assertion here will fail
 * @param c
 * @param others
 */
class Timer(c: TimerConfig, others: Seq[TimerConfig] = Seq()) extends Module {
  val io = IO(new TimerIO)
  io.trigger.driveDefaults()

  val othersOpt = others.filterNot(_ == c) // Filter out equal timers
  if (othersOpt.length > 0) {
    val otherTimerWidths = for (t <- others) yield log2Ceil(1 + Math.max(t.period, t.offset))

    val regCountdownOthers = RegInit(VecInit(Seq.tabulate(othersOpt.length)(i => othersOpt(i).offset.U(otherTimerWidths(i).W))))

    // Handle other timers
    for (i <- 0 until othersOpt.length) {
      regCountdownOthers(i) := regCountdownOthers(i) - 1.U

      when(regCountdownOthers(i) === 0.U) {
        assert(io.trigger.ready, "[Timer.scala] Absent Timer fired but downstream connection was busy")
        io.trigger.writeAbsent()
        io.trigger.fire := true.B
        regCountdownOthers(i) := othersOpt(i).period.U
      }
    }
  }

  val myTimerWidth = log2Ceil(1 + Math.max(c.offset, c.period))
  val regCountdownMe = RegInit(c.offset.U(myTimerWidth.W))
  // Handle this timer (we do it later because we can then overwrite absent tokens written by the other
  regCountdownMe := regCountdownMe - 1.U

  when (regCountdownMe === 0.U) {
    assert(io.trigger.ready, "[Timer.scala] Timer fired but downstream connection was busy")
    io.trigger.write(0.U)
    io.trigger.fire := true.B
    regCountdownMe := c.period.U
  }
}

class TimerBuilder(val offset: Int, val period: Int) {



  def

}

class GlobalTimer(cfgs: Seq[TimerConfig])
