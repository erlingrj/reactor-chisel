package reactor

import chisel3._
import chisel3.util._


case class EventQueueConfig[T <: Data](
  size: Int,
  gen: T,
) {
  require(isPow2(size), "[EventQueue] event queue size must be pow2")

  def addrBits = log2Ceil(size)
}

object EventQueueConfig {
  val Default = EventQueueConfig(8, UInt(8.W))
}



class EventQueueIO[T <: Data](c: EventQueueConfig[T])(implicit rc: ReactorGlobalParams) extends Bundle {
  val deq = Decoupled(new TaggedSignal(c.gen))
  val enq = Flipped(Decoupled(new TaggedSignal(c.gen)))

  val size = Output(UInt(c.addrBits.W))

  val sorting = Output(Bool())


  def tieOff() = {
    deq.bits := 0.U.asTypeOf(new TaggedSignal(c.gen))
    deq.valid := false.B
    enq.ready := false.B
    size := 0.U
    sorting := false.B
  }

}

abstract class EventQueue[T <: Data](c: EventQueueConfig[T])(implicit rc: ReactorGlobalParams) extends Module {
  val io = IO(new EventQueueIO(c))

  def getHead(): DecoupledIO[TaggedSignal[T]]
}



class OrderedRegQueue[T <: Data](c: EventQueueConfig[T])(implicit rc: ReactorGlobalParams) extends EventQueue[T](c) {

  val regQueue = RegInit(VecInit(Seq.fill(c.size)(0.U.asTypeOf(new TaggedSignal(c.gen)))))
  val regHeadPtr = RegInit(0.U(c.addrBits.W))
  val regTailPtr = RegInit(0.U(c.addrBits.W))

  val regSize = RegInit(0.U(c.addrBits.W))

  val regEnqueued = RegInit(0.U.asTypeOf(new TaggedSignal(c.gen)))
  val regReplaced = RegInit(0.U.asTypeOf(new TaggedSignal(c.gen)))

  val regSortIdx = RegInit(0.U(c.addrBits.W))
  val regSortFoundLoc = RegInit(false.B)

  val sReady :: sSorting :: nil = Enum(2)
  val regState = RegInit(sReady)


  override def getHead() = {
    val head = WireInit(0.U.asTypeOf(Decoupled(new TaggedSignal(c.gen))))
    when (regSize > 0.U) {
      head.bits := regQueue(regHeadPtr)
      head.valid := true.B
    }
    head
  }

  io.size := regSize

  switch (regState) {

    is (sReady) {
      io.enq.ready := regSize < c.size.U
      io.deq := getHead()

      val enq = WireInit(0.U(2.W))
      val deq = WireInit(0.U(2.W))

      when (io.enq.fire) {
        regState := sSorting
        regSortIdx := 0.U
        regEnqueued := io.enq.bits
        regSortFoundLoc := false.B
        enq := 1.U

      }
      when (io.deq.fire) {
        deq := 1.U
        regHeadPtr := regHeadPtr + 1.U
      }
      regSize := regSize + enq - deq
    }

    is (sSorting) {
      io.sorting := true.B
      val qIdx = regHeadPtr + regSortIdx

      when (regSize === 0.U) {
        assert(regHeadPtr === regTailPtr, "[EventQueue] Size = 0 but head != tail")
        regQueue(regHeadPtr) := regEnqueued
        regTailPtr := regTailPtr + 1.U
        regState := sReady
      }
      when (regSortFoundLoc) {
        regQueue(qIdx) := regReplaced
        regReplaced := regQueue(qIdx)
      }.otherwise {
        when (regEnqueued.tag.tag < regQueue(qIdx).tag.tag || regSortIdx === regSize) {
          regQueue(qIdx) := regEnqueued
          regReplaced := regQueue(qIdx)
          regSortFoundLoc := true.B
        }
      }

      regSortIdx := regSortIdx + 1.U

      when (regSortIdx === regSize) {
        regState := sReady
      }
    }
  }


  // Assertions
  when (regHeadPtr === regTailPtr) {
    assert(regSize === c.size.U || regSize === 0.U, "[EventQueue] headPtr === tailPtr but not full/empty")
  }

  when (RegNext(io.enq.fire)) {
    assert(regState === sSorting, "[EventQueue] enqueud, but did not move into sorting state")
  }

  when (regState === sSorting) {
    assert(io.sorting, "[EventQueue] in Sorting state but sorting signal not high")
    assert(!io.enq.fire, "[EventQueue] In sorting state but enqueuing")
    assert(!io.deq.fire, "[EventQueue] In sorting state but dequeuing")
  }
}