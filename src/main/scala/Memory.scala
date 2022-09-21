package reactor

import chisel3._
import chisel3.util._


case class MemoryConfig[+T <: Data](
  gen: T,
  nElems: Int,
  nReadPorts: Int,
  nWritePorts: Int,
)
{
  def nAddrBits: Int = if (nElems > 1) log2Ceil(nElems) else 1
  def nDataBits: Int = gen.getWidth
}

class MemReadIO[T<:Data](c: MemoryConfig[T]) extends Bundle {
  val addr = Input(UInt(c.nAddrBits.W))
  val data = Output(c.gen)
  val en = Input(Bool())
}

class MemWriteIO[T<:Data](c: MemoryConfig[T]) extends Bundle {
  val addr = Input(UInt(c.nAddrBits.W))
  val data = Input(c.gen)
  val en = Input(Bool())
}

class MemIO[T<:Data](c: MemoryConfig[T]) extends Bundle {
  val read = Vec(c.nReadPorts, new MemReadIO(c))
  val write = Vec(c.nWritePorts, new MemWriteIO(c))
}

abstract class Memory[T<:Data](c: MemoryConfig[T]) extends Module {
  val io = IO(new MemIO(c))
}
