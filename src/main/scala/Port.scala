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
  gen: T,
  useBram: Boolean,
) {
  def getPortIOConfig: PortIOConfig[T] = {
    PortIOConfig(
      gen = gen,
      nElems = nElems
    )
  }
  def getMemConfig: MemoryConfig[T] = {
    MemoryConfig(
      gen = gen,
      nElems=nElems,
      nReadPorts = nReaders,
      nWritePorts = 1
    )
  }
}

case class PortIOConfig[+T <: Data](
  nElems: Int,
  gen: T
) {
  def nAddrBits: Int = if (nElems > 1) log2Ceil(nElems) else 1
  def nDataBits: Int = gen.getWidth
}

// PortIn is a antiDependency
class PortInIO[T <: Data](c: PortIOConfig[T]) extends Bundle {
  val en = Input(Bool())
  val addr = Input(UInt(c.nAddrBits.W))
  val data = Input(c.gen)

  def reactionTieOff: Unit = {
    en := false.B
    addr := 0.U
   data := 0.U
  }

  def write[T <: Data](address: UInt, dat: T) = {
    en := true.B
    addr := address
    data := dat
  }
}

// PortOut is a trigger/dependency
class PortOutIO[T <: Data](c: PortIOConfig[T]) extends Bundle {
  val present = Output(Bool())
  val addr = Input(UInt(c.nAddrBits.W))
  val en = Input(Bool())
  val data = Output(c.gen)

  def reactionTieOff: Unit = {
    addr := 0.U
    en := false.B
  }
  def read(address: UInt) = {
    en := true.B
    addr := address
  }
  def readRsp: T = {
    WireInit(data)
  }
}

class PortIO[T <: Data](c: PortConfig[T]) extends Bundle {
  val in = new PortInIO(c.getPortIOConfig)
  val outs = Vec(c.nReaders, new PortOutIO(c.getPortIOConfig))
  val evalEnd = Input(Bool())
}

class Port[T <: Data](c: PortConfig[T]) extends Module {
  require(!c.useBram)
  val io = IO(new PortIO(c))

  val regPresent = RegInit(false.B)
  io.outs.map(_.present := regPresent)

  // A port contains an memory (which can be Register-based or BRAM based)
  val mem: Memory[T] =
    if (c.useBram) Module(new MemoryBram(c.getMemConfig))
    else Module(new MemoryReg(c.getMemConfig))

  mem.io.read zip io.outs foreach {
    case (m,p) => {
      m.en := p.en
      m.addr := p.addr
      p.data := m.data
  }}

  mem.io.write(0).data := io.in.data
  mem.io.write(0).en := io.in.en
  mem.io.write(0).addr := io.in.addr


  // When something is written to the Port we should output the present signal
  when(io.in.en) {
    regPresent := true.B
  }

  // When we reach the end of an evaluation cycle. Scheduler will signal evalEnd to all Ports to reset present signal
  when(io.evalEnd) {
    regPresent := false.B
  }

  assert(!(io.in.en && io.evalEnd), "[Port.scala] reaction tries to write to port while scheduler says eval end")
  assert(!(io.in.en && io.outs.map(_.en).reduce(_||_)), "Port.scala read and write to port at the same time")
}
