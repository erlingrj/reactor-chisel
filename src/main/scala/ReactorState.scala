//package reactor
//
//import chisel3._
//import chisel3.util._
//
//
//case class ReactorStateConfig[+T <: Data](
//  gen: T,
//  nElems: Int,
//  useBram: Boolean
//) {
//  def nAddrBits: Int = if (nElems > 1) log2Ceil(nElems) else 1
//  def nDataBits: Int = gen.getWidth
//  def getMemoryConfig: MemoryConfig[T] =
//    MemoryConfig(
//      gen = gen, nElems = nElems, nReadPorts = 1, nWritePorts = 1
//    )
//}
//
//class ReactorStateIO[T <: Data](c: ReactorStateConfig[T]) extends Bundle {
//  val readPort = new MemReadIO(c.getMemoryConfig)
//  val writePort = new MemWriteIO(c.getMemoryConfig)
//
//  def read(addr: UInt) = {
//    readPort.en := true.B
//    readPort.addr := addr
//  }
//
//  def readRsp = {
//    readPort.data
//  }
//
//  def write(addr: UInt, data: T): Unit = {
//    writePort.data := data
//    writePort.en := true.B
//    writePort.addr := addr
//  }
//
//  def tieOffExt = {
//    readPort.addr := 0.U
//    readPort.en := false.B
//    writePort.en := false.B
//    writePort.data := 0.U
//    writePort.addr := 0.U
//  }
//
//
//}
//
//class ReactorState[T <: Data](c: ReactorStateConfig[T]) extends Module {
//  require(!c.useBram)
//  val io = IO(new ReactorStateIO(c))
//
//  val mem: Memory[T] =
//    if (c.useBram) Module(new MemoryBram(c.getMemoryConfig))
//    else Module(new MemoryReg(c.getMemoryConfig))
//
//  mem.io.read(0) <> io.readPort
//  mem.io.write(0) <> io.writePort
//}
//
