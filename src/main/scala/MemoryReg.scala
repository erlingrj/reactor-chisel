//package reactor
//
//import chisel3._
//import chisel3.util._
//
//
//// MemoryReg supports 1WnR ports
//class MemoryReg[T<:Data](c: MemoryConfig[T]) extends Memory(c) {
//  require(c.nWritePorts == 1)
//  require(c.nReadPorts > 0)
//
//  val data = RegInit(VecInit(Seq.fill(c.nElems)(0.U.asTypeOf(c.gen))))
//  val readBufs = RegInit(VecInit(Seq.fill(c.nReadPorts)(0.U.asTypeOf(c.gen))))
//
//  // Reading
//  io.read zip readBufs foreach{ case (port, buf) => port.data := buf }
//
//  for (readIdx <- 0 until c.nReadPorts) {
//    when (io.read(readIdx).en) {
//      readBufs(readIdx) := data(io.read(readIdx).addr)
//    }
//  }
//
//  // Writing
//  when(io.write(0).en) {
//    data(io.write(0).addr) := io.write(0).data
//  }
//}
