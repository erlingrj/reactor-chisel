package reactor

import chisel3._
import chisel3.util._


class ActionIO[T <: Data](c : ActionConfig[T])(implicit rc: ReactorGlobalParams) extends Bundle {
  val nextEvent = Valid(new TaggedSignal)
  val outPort = Decoupled(new TaggedSignal)
  val schedule = Flipped(Decoupled(new TaggedSignal))
  val busy = Output(Bool())

  val logicalTime = Flipped(Valid(Tag()))

  def tieOff() = {
    outPort.bits := 0.U.asTypeOf(new TaggedSignal)
    outPort.valid := false.B
    schedule.ready := false.B
    busy := false.B
  }
}

class Action[T <: Data](c: ActionConfig[T])(implicit rc: ReactorGlobalParams) extends ReactorElement(id = c.id) {
  val io = IO(new ActionIO(c))
  val eventQ = Module(new OrderedRegQueue[T](c.eventQueueConfig)).io

  val regLogicalTime = RegInit(Tag(0.U))

  // Outport
  io.outPort.bits.value := eventQ.deq.bits.value
  io.outPort.valid := eventQ.deq.valid && regLogicalTime.tag === eventQ.deq.bits.tag.tag
  eventQ.deq.ready := io.outPort.ready

  // Next event
  io.nextEvent.valid := eventQ.deq.valid
  io.nextEvent.bits := eventQ.deq.bits

  // Schedule
  eventQ.enq <> io.schedule

  // Logical time
  io.busy := eventQ.sorting
  when (io.logicalTime.fire) {
    regLogicalTime := io.logicalTime.bits
  }


  when (eventQ.deq.valid) {
    assert(regLogicalTime.tag <= eventQ.deq.bits.tag.tag, "[Action] LogicalTime is greater than tag in EventQ")
  }
}

class TimerIO(tc: TimerConfig)(implicit rc: ReactorGlobalParams) extends Bundle {
  val nextEvent = Valid(new Tag)
  val outPort = Decoupled(new EmptySignal)
}

class Timer[T <: Data](c: TimerConfig)(implicit rc: ReactorGlobalParams) extends ReactorElement(id = c.id) {
  val io = IO(new TimerIO(c))
}


