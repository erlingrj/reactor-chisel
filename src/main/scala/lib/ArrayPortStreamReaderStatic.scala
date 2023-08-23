package reactor.lib

import reactor._

import chisel3._
import chisel3.util._

class ArrayPortStreamReader[T <: Data](port: EventArrayReadMaster[T]) extends Module {}

class ArrayPortStreamReaderIO[T <: Data](genData: T, genToken: ArrayToken[T]) extends Bundle {
  val _port = new EventArrayReadMaster(genData, genToken)
  val read = Decoupled(genData)
  val start = Input(Bool())
  val done = Output(Bool())

  def driveDefaults() = {
    done := false.B
    _port.driveDefaults()
    read.valid := false.B
    read.bits := 0.U.asTypeOf(genData)
  }
  def driveDefaultsFlipped() = {
    start := false.B
    read.ready := false.B
  }
}
class ArrayPortStreamReaderStaticIO[T <: Data](genData: T, genToken: ArrayToken[T]) extends ArrayPortStreamReaderIO(genData, genToken){}

class ArrayPortStreamReaderStatic[T <: Data](port: EventArrayReadMaster[T], readAddrSeq: Seq[Int]) extends ArrayPortStreamReader(port){
  val genData = port.genData
  val genToken = port.genToken

  def readAddrVec = VecInit(readAddrSeq.map(_.U))

  val io = IO(new ArrayPortStreamReaderStaticIO(genData, genToken))
  io.driveDefaults()

  val regCnt = RegInit(0.U((log2Ceil(readAddrSeq.length)+1).W))
  val regData = RegInit(0.U.asTypeOf(genData))
  val regDataValid = RegInit(false.B)
  val regMemRespValid = RegInit(false.B)

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  io.read.valid := regDataValid
  io.read.bits := regData

  switch(regState) {
    is (sIdle) {
      when (io.start) {
        regState := sRunning
        regMemRespValid := false.B
        regDataValid := false.B
        regDataValid := 0.U.asTypeOf(genData)
        regCnt := 0.U
      }
    }

    is (sRunning) {
      io._port.req.addr := readAddrVec(regCnt)
      regMemRespValid := true.B

      when(regMemRespValid && (io.read.fire || !regDataValid)) {
        regData := io._port.resp.token.data
        regDataValid := true.B
        regCnt := regCnt + 1.U
      }

      when(!regMemRespValid) {
        regCnt := regCnt + 1.U
      }

      when(io.read.fire && regCnt === readAddrSeq.length.U) {
        regState := sDone
      }
    }

    is (sDone) {
      io.done := true.B
    }
  }

  when(!io.start) {
    regState := sIdle
  }

  assert(!(regState === sRunning && !io.start))
}

object ArrayPortStreamReaderStatic {
  def apply[T <: Data](port: EventArrayReadMaster[T], readSeq: Seq[Int]) : ArrayPortStreamReaderStaticIO[T] = {
    val r = Module(new ArrayPortStreamReaderStatic(port, readSeq))
    r.io.driveDefaultsFlipped()
    r.io._port <> port
    r.io
  }
}