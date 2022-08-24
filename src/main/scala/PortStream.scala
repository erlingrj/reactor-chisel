package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, Direction}


class PortStreamReaderIO[T <: Data](c: PortIOConfig, gen: T) extends Bundle {
  val portRead = Flipped(new PortOutIO(c,gen))
  val start = Flipped(Decoupled())
  val done = Output(Bool())

  val out = Decoupled(gen)

  def tieOff(): Unit = {
      portRead.addr := 0.U
      portRead.en := false.B
      start.ready := false.B
      done := false.B
    out.valid := false.B
    out.bits := 0.U.asTypeOf(gen)
    }
  def tieOffExt(): Unit = {
    start.valid := false.B
    out.ready := false.B
  }
}
class PortStreamReader[T <: Data](c: PortIOConfig, gen: T) extends Module {
  val io = IO(new PortStreamReaderIO(c,gen))
  io.tieOff()

  val regCnt = RegInit(0.U((c.nAddrBits+1).W))
  val regRspValid = RegInit(false.B)
  val wReadEn = WireInit(false.B)
  wReadEn := false.B

  regRspValid := wReadEn

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val queueSize = 4
  val queue = Module(new Queue(gen, queueSize, useSyncReadMem = true))
  queue.io.deq <> io.out
  queue.io.enq.bits := io.portRead.data
  queue.io.enq.valid := regRspValid
  assert(!(regRspValid && !queue.io.enq.ready), "[Port.scala] PortStreamReader read data but queue was not ready")

  val regState = RegInit(sIdle)

  switch(regState) {
    is (sIdle) {
      io.start.ready := true.B
      when (io.start.fire) {
        regState := sRunning
        regCnt := 0.U
      }
    }

    is (sRunning) {
      when(regCnt === (c.nElems).U) {
        regState := sDone
        assert(queue.io.enq.fire, "[Port.scala] PortStreamReader should have enqueued last value")
      } otherwise {
        // Guarantee that we will have room for the data when it arrives next cycle
        when(queue.io.count < (queueSize - 2).U) {
          io.portRead.en := true.B
          wReadEn := true.B
          io.portRead.addr := regCnt

          regCnt := regCnt + 1.U
        }
      }
    }

    is (sDone) {
      when(queue.io.count === 0.U) {
        io.done := true.B
        regState := sIdle
        regCnt := 0.U
      }
    }
  }
}

class PortStreamWriterIO[T <: Data](c: PortIOConfig, gen: T) extends Bundle {
  val in = Flipped(Decoupled(gen))
  val portWrite = Flipped(new PortInIO(c,gen))
  val done = Output(Bool())
  val active = Output(Bool())

  def tieOff(): Unit = {
      in.ready := false.B
      portWrite.addr := 0.U
      portWrite.en := false.B
      portWrite.data := 0.U
      done := false.B
      active := false.B
    }
  def tieOffExt(): Unit = {
    in.valid := false.B
    in.bits := 0.U.asTypeOf(in.bits)
  }
}
class PortStreamWriter[T <: Data](c: PortIOConfig, gen: T) extends Module {
  val io = IO(new PortStreamWriterIO(c,gen))
  io.tieOff()

  val regCnt = RegInit(0.U(c.nAddrBits.W))
  val regDone = RegInit(false.B)
  regDone := false.B
  io.done := regDone
  io.active := (regCnt > 0.U)

  // Pipe the input data to reduce combinatorial paths
  io.in.ready := true.B
  val pipedIn = Pipe(io.in.valid, io.in.bits)

  when (pipedIn.valid) {
    io.portWrite.addr := regCnt
    io.portWrite.data := pipedIn.bits
    io.portWrite.en := true.B

    when(regCnt === (c.nElems-1).U) {
      regCnt := 0.U
      regDone := true.B
    } otherwise {
      regCnt := regCnt + 1.U
    }
  }
}

