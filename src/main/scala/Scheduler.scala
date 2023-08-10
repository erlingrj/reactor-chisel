package reactor

import chisel3._
import chisel3.util._

// The Scheduler monitors the following signals and decides when to fire of events
// NET: This comes from the Schedule and is the tag of the head of the event queue
// TAG: This comes from the top-level IO (in case of co-design).

// Enum of the different types of event modes. Either we have only local events (from our own EventQueue) or
// we have only events from the SW. Or we have both.
object EventMode extends ChiselEnum {
  val noEvent, localOnly, externalOnly, localAndExternal = Value

  def hasLocalEvent(eventMode: UInt): Bool = eventMode.asBools(0)
  def hasExternalEvent(eventMode: UInt): Bool = eventMode.asBools(1)
}


class SchedulerIO extends Bundle {
  val execute = Decoupled(new ExecuteIO())
  val nextEventTag = Input(Tag())
  val tagAdvanceGrant = Input(Tag())
  val swInputPresent = Input(Bool())
  val now = Input(Tag())

  def trigger = execute.valid
  def driveDefaults(): Unit = {
    execute.valid := false.B
    execute.bits := 0.U.asTypeOf(execute.bits)
  }
}

class Scheduler extends Module {
  val io = IO(new SchedulerIO())
  io.driveDefaults()

  val localEventReady = io.now >= io.nextEventTag && io.tagAdvanceGrant >= io.nextEventTag
  val externalEventReady = io.now >= io.tagAdvanceGrant && io.tagAdvanceGrant <= io.nextEventTag && io.swInputPresent

  val sIdle :: sFiring :: Nil = Enum(2)
  val regState = RegInit(sIdle)

  switch(regState) {
    is (sIdle) {
      when (localEventReady || externalEventReady) {
        regState := sFiring
      }
    }
    is (sFiring) {
      io.execute.valid := true.B
      when(localEventReady && externalEventReady) {
        io.execute.bits.eventMode := EventMode.localAndExternal
        io.execute.bits.tag := io.nextEventTag
      }.elsewhen(localEventReady) {
        io.execute.bits.eventMode := EventMode.localOnly
        io.execute.bits.tag := io.nextEventTag
      }.elsewhen(externalEventReady) {
        io.execute.bits.eventMode := EventMode.externalOnly
        io.execute.bits.tag := io.tagAdvanceGrant
      }.otherwise {
        io.execute.bits.eventMode := EventMode.noEvent
        io.execute.bits.tag := 0.U
      }
      when (io.execute.ready) {
        regState := sIdle
      }
    }
  }
}