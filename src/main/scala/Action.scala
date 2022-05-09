package reactor

import chisel3._
import chisel3.util._

/**
 * An Action is an event-queue with future scheduled events.
 * It has a schedule input which Reactions can use
 * It has a nextEvent output which goes to the local ClockKeeper
 * The OutPort is connected to the Arbiter and holds the value of the event
 * LogicalTime input
 *
 */

class ActionIO[T <: Data](c : ActionConfig[T])(implicit rc: ReactorGlobalParams) extends Bundle {
  val nextEvent = Decoupled(TimeTag())
  val outPort = Decoupled(TaggedSignal(c.gen))
  val schedule = Flipped(Decoupled(TaggedSignal(c.gen)))
  val busy = Output(Bool())

  val logicalTime = Flipped(Decoupled(TimeTag()))

  def tieOff() = {
    outPort.bits := 0.U.asTypeOf(TaggedSignal(c.gen))
    outPort.valid := false.B
    schedule.ready := false.B
    logicalTime.ready := false.B
    busy := false.B
  }
}

class Action[T <: Data](c: ActionConfig[T])(implicit rc: ReactorGlobalParams) extends ReactorElement {
  val io = IO(new ActionIO(c))
  io.tieOff()

  val eventQ = Module(new OrderedRegQueue[T](c.eventQueueConfig)).io
  eventQ.deq.ready := false.B

  val regLogicalTime = RegInit(TimeTag(0.U))
  val regOutBuf = RegInit(TaggedSignal(c.gen, 0.U,TimeTag(0.U)))
  val regFired = RegInit(0.U(log2Ceil(c.numAntiDependencies+1).W))

  val sWait :: sSort :: sExecute :: Nil = Enum(3)
  val regState = RegInit(sWait)

  // Next event
  io.nextEvent.valid := eventQ.deq.valid
  io.nextEvent.bits := eventQ.deq.bits.tag

  // Schedule
  eventQ.enq <> io.schedule

  switch (regState) {
    is(sWait) {
      // Logical time
      io.busy := eventQ.sorting
      io.logicalTime.ready := !eventQ.sorting
      when (io.logicalTime.fire) {
        regLogicalTime := io.logicalTime.bits

        when (io.logicalTime.bits=== eventQ.deq.bits.tag && eventQ.deq.valid) {
          regState := sExecute
          regOutBuf := eventQ.deq.bits
          regFired := 0.U
          eventQ.deq.ready := true.B

          assert(eventQ.deq.fire, "[Action.scala] eventQ should have fired")
        }
      }
    }

    is (sExecute) {
      io.busy := true.B
      io.outPort.bits := regOutBuf
      io.outPort.valid := true.B
      when (io.outPort.fire) {
        regFired := regFired + 1.U
        when (regFired === (c.numAntiDependencies-1).U) {
          regOutBuf := TaggedSignal(c.gen, 0.U, TimeTag(0.U))
          regState := sWait
          regFired := 0.U
        }
      }
    }
  }



  when (eventQ.deq.valid) {
    assert(regLogicalTime <= eventQ.deq.bits.tag, "[Action] LogicalTime is greater than tag in EventQ")
  }
}
