package reactor

import chisel3._
import chisel3.util._

class TimerIO extends Bundle {
  val trigger = new EventWriteMaster(UInt(0.W), new PureToken())
}
class Timer(offset: Int, period: Int) extends Module {
  val io = IO(new TimerIO)
  io.trigger.driveDefaults()

  val timerWidth = if (period > offset) log2Ceil(period) else log2Ceil(offset)

  val regCountdown = RegInit(offset.U(timerWidth.W))

  regCountdown := regCountdown - 1.U

  when (regCountdown === 0.U) {
    assert(io.trigger.ready, "[Timer.scala] Timer fired but downstream connection was busy")

    io.trigger.req.valid := true.B
    io.trigger.req.present := true.B
    io.trigger.req.token := 0.U.asTypeOf(new PureToken)
    io.trigger.fire := true.B

    regCountdown := period.U
  }
}
