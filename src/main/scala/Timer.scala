package reactor

import chisel3._
import chisel3.util._

class TimerIO(c: TimerConfig)(implicit rc: ReactorGlobalParams) extends Bundle {
  val nextEvent = Decoupled(TimeTag())
  val outPort = Decoupled()

  val logicalTime = Flipped(Decoupled(TimeTag()))

  def tieOff = {
    nextEvent.valid := false.B
    nextEvent.bits := TimeTag(0.U)
    outPort.valid := false.B
    logicalTime.ready := false.B
  }
}

class Timer(c: TimerConfig)(implicit rc: ReactorGlobalParams) extends ReactorElement {
  val io = IO(new TimerIO(c))
  io.tieOff

  val regLogicalTime = RegInit(TimeTag(0.U))
  val regNextEvent = RegInit(TimeTag(c.offset.U))
  val regFired = RegInit(0.U(log2Ceil(c.numAntiDependencies+1).W))

  val sWait :: sExecute :: Nil = Enum(2)
  val regState = RegInit(sWait)

  io.nextEvent.valid := true.B
  io.nextEvent.bits := regNextEvent

  switch(regState) {
    is(sWait) {
      io.logicalTime.ready := true.B
      when(io.logicalTime.fire) {
        regLogicalTime := io.logicalTime.bits
        when (io.logicalTime.bits === regNextEvent) {
          regState := sExecute
        }
        assert(!(io.logicalTime.bits > regNextEvent), "[Timer.scala] Received logical time greater than our current nextEvent")
        assert(regFired === 0.U, "[Timer.scala] in sWait but regFired != 0")
      }
    }

    is (sExecute) {
      io.nextEvent.valid := false.B
      io.outPort.valid := true.B
      when (io.outPort.fire) {
        regFired := regFired + 1.U
        when (regFired === (c.numAntiDependencies - 1).U) {
          regState := sWait
          regNextEvent := regNextEvent + c.interval.U
          regFired := 0.U
        }
      }
    }
  }






}
