package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror

/**
 *  Ports are what connects Reactors to Reactor
 *  OutputPort
 */


// TODO: The difference between PortConfig and PortIOConfig is not that clear. Revisit
case class PortConfig[T <: Data](
  nElems: Int,
  nReaders: Int,
  gen: T
) {
  def getPortIOConfig: PortIOConfig[T] = PortIOConfig(nElems,gen)
}

case class PortIOConfig[+T <: Data](
  nElems: Int,
  gen: T
) {
  def nAddrBits: Int = if (nElems > 1) log2Ceil(nElems) else 1
  def nDataBits: Int = gen.getWidth
}

// PortIn is a antiDependency
class PortInIO[+T <: Data](c: PortIOConfig[T]) extends Bundle {
  val en = Input(Bool())
  val addr = Input(UInt(c.nAddrBits.W))
  val data = Input(c.gen)

  def reactionTieOff: Unit = {
    en := false.B
    addr := 0.U
   data := 0.U
  }
}

// PortOut is a trigger/dependency
class PortOutIO[+T <: Data](c: PortIOConfig[T]) extends Bundle {
  val present = Output(Bool())
  val addr = Input(UInt(c.nAddrBits.W))
  val en = Input(Bool())
  val data = Output(c.gen)

  def reactionTieOff: Unit = {
    addr := 0.U
    en := false.B
  }
}

class PortIO[T <: Data](c: PortConfig[T]) extends Bundle {
  val in = new PortInIO(c.getPortIOConfig)
  val outs = Vec(c.nReaders, new PortOutIO(c.getPortIOConfig))
  val evalEnd = Input(Bool())
}

abstract class Port[T <: Data](c: PortConfig[T]) extends Module {
  val io = IO(new PortIO(c))

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


class BramPort[T <: Data](c: PortConfig[T]) extends Port[T](c) {

}

class DramPort[T <: Data](c: PortConfig[T]) extends Port[T](c) {

}

class RegPort[T <: Data](c: PortConfig[T]) extends Port[T](c) {

  val data = RegInit(VecInit(Seq.fill(c.nElems)(0.U.asTypeOf(c.gen))))
  val readBufs = RegInit(VecInit(Seq.fill(c.nReaders)(0.U.asTypeOf(c.gen))))

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


