package reactor

import chisel3._

class MemoryBram[T<:Data](c: MemoryConfig[T]) extends Memory(c) {
  require(false)
  require(c.nWritePorts == 1)
  require(c.nReadPorts  == 1)
}
