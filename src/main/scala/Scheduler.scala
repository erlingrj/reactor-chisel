package reactor

import chisel3._
import chisel3.util._

// The Scheduler monitors the following signals and decides when to fire of events
// NET: This comes from the Schedule and is the tag of the head of the event queue
// TAG: This comes from the top-level IO (in case of co-design).

// Enum of the different types of event modes. Either we have only local events (from our own TokenQueue) or
// we have only events from the SW. Or we have both.
object EventMode extends ChiselEnum {
  val noEvent, localOnly, externalOnly, localAndExternal = Value

  def hasLocalEvent(eventMode: UInt): Bool = eventMode.asBools(0)
  def hasExternalEvent(eventMode: UInt): Bool = eventMode.asBools(1)
}


class SchedulerIO extends Bundle {
  val execute = Decoupled(new ExecuteIO())
  val nextEventTag = Input(Valid(Tag()))
  val tagAdvanceGrant = Input(Valid(Tag()))
  val swInputPresent = Input(Bool())
  val now = Input(Tag())

  def trigger = execute.valid
  def driveDefaults(): Unit = {
    execute.valid := false.B
    execute.bits := 0.U.asTypeOf(execute.bits)
  }
}

class Scheduler(implicit val cfg: GlobalReactorConfig) extends Module {
  val io = IO(new SchedulerIO())
  io.driveDefaults()
  def localEventReady =
    if (cfg.standalone)
      io.nextEventTag.valid && io.now >=io.nextEventTag.bits
    else
      io.tagAdvanceGrant.valid && io.nextEventTag.valid && io.now >= io.nextEventTag.bits && io.tagAdvanceGrant.bits >= io.nextEventTag.bits
  def externalEventReady =
    if (cfg.standalone)
      false.B
    else
      io.tagAdvanceGrant.valid && io.nextEventTag.valid && io.now >= io.tagAdvanceGrant.bits && io.tagAdvanceGrant.bits <= io.nextEventTag.bits && io.swInputPresent

  assert(!(io.tagAdvanceGrant.valid && io.tagAdvanceGrant.bits > io.nextEventTag.bits && io.tagAdvanceGrant.bits =/= Tag.FOREVER), "[Scheduler] TAG > NET not allowed yet")
  val sIdle :: sFiring :: sWaitForNET :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  // FIXME: Consider adding a register for the io.execute.bits.tag to read from instead of getting it straight from outside
  switch(regState) {
    is (sIdle) {
      when ((localEventReady || externalEventReady)) {
        regState := sFiring
      }
    }
    is (sFiring) {
      io.execute.valid := true.B
      when(localEventReady && externalEventReady) {
        io.execute.bits.eventMode := EventMode.localAndExternal
        io.execute.bits.tag := io.nextEventTag.bits
      }.elsewhen(localEventReady) {
        io.execute.bits.eventMode := EventMode.localOnly
        io.execute.bits.tag := io.nextEventTag.bits
      }.elsewhen(externalEventReady) {
        io.execute.bits.eventMode := EventMode.externalOnly
        io.execute.bits.tag := io.tagAdvanceGrant.bits
      }.otherwise {
        io.execute.bits.eventMode := EventMode.noEvent
        io.execute.bits.tag := 0.U
      }

      when(io.execute.fire) {
        regState := sWaitForNET
      }
    }
    is (sWaitForNET) {
      when (io.execute.ready) {
        regState := sIdle
      }
    }
  }
}