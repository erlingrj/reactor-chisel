//package reactor.lib
//
//import chisel3._
//import chisel3.util._
//import reactor._
//
//class ArrayPortStreamWriter[T <: Data](port: ArrayTokenWriteMaster[T]) extends Module {}
//
//class ArrayPortStreamWriterIO[T <: Data](genData: T, genToken: ArrayToken[T]) extends Bundle {
//  val _port = new ArrayTokenWriteMaster(genData, genToken)
//  val write = Flipped(Decoupled(genData))
//  val start = Input(Bool())
//  val done = Output(Bool())
//
//  def driveDefaults() = {
//    done := false.B
//    _port.driveDefaults()
//    write.ready := false.B
//  }
//  def driveDefaultsFlipped() = {
//    start := false.B
//    write.valid := false.B
//    write.bits := 0.U.asTypeOf(genData)
//  }
//}
//class ArrayPortStreamWriterStaticIO[T <: Data](genData: T, genToken: ArrayToken[T]) extends ArrayPortStreamWriterIO(genData, genToken){}
//
//class ArrayPortStreamWriterStatic[T <: Data](port: ArrayTokenWriteMaster[T], writeAddrSeq: Seq[Int]) extends ArrayPortStreamWriter(port){
//  val genData = port.genData
//  val genToken = port.genToken
//
//  def writeAddrVec = VecInit(writeAddrSeq.map(_.U))
//
//  val io = IO(new ArrayPortStreamWriterStaticIO(genData, genToken))
//  io.driveDefaults()
//
//  val regCnt = RegInit(0.U((log2Ceil(writeAddrSeq.length)+1).W))
//
//  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
//  val regState = RegInit(sIdle)
//
//  switch(regState) {
//    is (sIdle) {
//      when (io.start) {
//        regState := sRunning
//        regCnt := 0.U
//      }
//    }
//
//    is (sRunning) {
//      io._port.req.addr := writeAddrVec(regCnt)
//      io.write.ready := true.B
//
//      when(io.write.fire) {
//        io._port.req.present := true.B
//        io._port.req.valid := true.B
//        io._port.req.token.data := io.write.bits
//        regCnt := regCnt + 1.U
//      }
//
//      when(io.write.fire && regCnt === (writeAddrSeq.length-1).U) {
//        regState := sDone
//      }
//    }
//
//    is (sDone) {
//      io.done := true.B
//    }
//  }
//
//  when(!io.start) {
//    regState := sIdle
//  }
//
//  assert(!(regState === sRunning && !io.start))
//}
//
//object ArrayPortStreamWriterStatic {
//  def apply[T <: Data](port: ArrayTokenWriteMaster[T], writeSeq: Seq[Int]) : ArrayPortStreamWriterStaticIO[T] = {
//    val w = Module(new ArrayPortStreamWriterStatic(port, writeSeq))
//    w.io.driveDefaultsFlipped()
//    w.io._port <> port
//    w.io
//  }
//}