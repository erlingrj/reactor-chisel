package reactor

import chisel3._
import chisel3.util._

/**
 *  Ports are what connects Reactors to Reactor
 *  OutputPort
 */


case class PortConfig(
  nElems: Int,
  nReaders: Int
) {
  def nAddrBits = log2Ceil(nElems)
}

class PortInIO[T <: Data](c: PortConfig, gen: T) extends Bundle {
  val wen = Input(Bool())
  val addr = Input(UInt(c.nAddrBits.W))
  val data = Input(gen)
}


class PortOutIO[T <: Data](c: PortConfig, gen: T) extends Bundle {
  val present = Output(Bool())
  val addr = Input(UInt(c.nAddrBits.W))
  val data = Output(gen)
}

class PortIO[T <: Data](c: PortConfig, gen: T) extends Bundle {
  val in = new PortInIO(c,gen)
  val outs = Vec(c.nReaders, new PortOutIO(c,gen))
  val evalEnd = Input(Bool())
}

abstract class Port[T <: Data](c: PortConfig, gen: T) extends Module {
  val io = IO(new PortIO(c,gen))

  val regPresent = RegInit(false.B)
  io.outs.map(_.present := regPresent)

  // When something is written to the Port we should output the present signal
  when(io.in.wen) {
    regPresent := true.B
  }

  // When we reach the end of an evaluation cycle. Scheduler will signal evalEnd to all Ports to reset present signal
  when(io.evalEnd) {
    regPresent := false.B
  }

  assert(!(io.in.wen && io.evalEnd), "[Port.scala] reaction tries to write to port while scheduler says eval end")
  assert(!(io.in.wen && io.outs.map(_.addr =/= 0.U).reduce(_||_)), "Port.scala read and write to port at the same time")
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
    readBufs(readIdx) := data(io.outs(readIdx).addr)
  }

  // Writing
  when(io.in.wen) {
    data(io.in.addr) := io.in.data
  }
}