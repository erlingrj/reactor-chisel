package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror

/**
 *  Ports are what connects Reactors to Reactor
 *  OutputPort
 */


case class PortConfig(
  nElems: Int,
  nReaders: Int
) {
  def getPortIOConfig: PortIOConfig = PortIOConfig(nElems)
}

case class PortIOConfig(
  nElems: Int,
) {
  def nAddrBits: Int = log2Ceil(nElems)
}
// PortIn is a antiDependency
class PortInIO[T <: Data](c: PortIOConfig, gen: T) extends Bundle {
  val en = Input(Bool())
  val addr = Input(UInt(c.nAddrBits.W))
  val data = Input(gen)

  def reactionTieOff: Unit = {
    en := false.B
    addr := 0.U
    data := 0.U
  }
}

// PortOut is a trigger/dependency
class PortOutIO[T <: Data](c: PortIOConfig, gen: T) extends Bundle {
  val present = Output(Bool())
  val addr = Input(UInt(c.nAddrBits.W))
  val en = Input(Bool())
  val data = Output(gen)

  def reactionTieOff: Unit = {
    addr := 0.U
    en := false.B
  }
}

class PortIO[T <: Data](c: PortConfig, gen: T) extends Bundle {
  val in = new PortInIO(c.getPortIOConfig,gen)
  val outs = Vec(c.nReaders, new PortOutIO(c.getPortIOConfig,gen))
  val evalEnd = Input(Bool())
}

abstract class Port[T <: Data](c: PortConfig, gen: T) extends Module {
  val io = IO(new PortIO(c,gen))

  val regPresent = RegInit(false.B)
  io.outs.map(_.present := regPresent)

  // When something is written to the Port we should output the present signal
  when(io.in.en) {
    regPresent := true.B
  }

  // When we reach the end of an evaluation cycle. Scheduler will signal evalEnd to all Ports to reset present signal
  when(io.evalEnd) {
    regPresent := false.B
  }

  assert(!(io.in.en && io.evalEnd), "[Port.scala] reaction tries to write to port while scheduler says eval end")
  assert(!(io.in.en && io.outs.map(_.addr =/= 0.U).reduce(_||_)), "Port.scala read and write to port at the same time")
}


class BramPort[T <: Data](c: PortConfig, gen:T) extends Port[T](c, gen) {

}

class DramPort[T <: Data](c: PortConfig, gen: T) extends Port[T](c, gen) {

}

class RegPort[T <: Data](c: PortConfig, gen: T) extends Port[T](c, gen) {

  val data = RegInit(VecInit(Seq.fill(c.nElems)(0.U.asTypeOf(gen))))
  val readBufs = RegInit(VecInit(Seq.fill(c.nReaders)(0.U.asTypeOf(gen))))

  // Reading
  io.outs zip readBufs map({ case (port, buf) => port.data := buf })

  for (readIdx <- 0 until c.nReaders) {
    when (io.outs(readIdx).en) {
      readBufs(readIdx) := data(io.outs(readIdx).addr)
    }
  }

  // Writing
  when(io.in.en) {
    data(io.in.addr) := io.in.data
  }
}

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
}
class PortStreamReader[T <: Data](c: PortIOConfig, gen: T) extends Module {
  val io = IO(new PortStreamReaderIO(c,gen))
  io.tieOff()

  val regCnt = RegInit(0.U(c.nAddrBits.W))
  val regRspValid = RegInit(false.B)
  val wReadEn = WireInit(false.B)
  wReadEn := false.B

  regRspValid := wReadEn

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val queueSize = 4
  val queue = Module(new Queue(gen, queueSize))
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
      when(regCnt === c.nElems.U) {
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
  val start = Flipped(Decoupled())
  val done = Output(Bool())
}
class PortStreamWriter[T <: Data](c: PortIOConfig, gen: T) extends Module {
  val io = IO(new PortStreamWriterIO(c,gen))
}
