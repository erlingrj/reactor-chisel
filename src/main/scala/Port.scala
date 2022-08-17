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