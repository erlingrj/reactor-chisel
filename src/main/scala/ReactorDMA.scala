package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import fpgatidbits.dma._
import fpgatidbits.PlatformWrapper._

import scala.math.ceil


case class ReactorDMAConfig(
  inPorts : MixedVec[PortIOConfig[Data]],


  nElemsIn : Seq[Int],
  nElemsOut : Seq[Int],
  nInputPorts: Int,
  nOutputPorts: Int,
  mrp : MemReqParams
) {
  require(nInputPorts > 0)
  require(nInputPorts == nElemsIn.length)
  require(nOutputPorts > 0)
  require(nOutputPorts == nElemsOut)
}

class DmaTransactionInfo(p: MemReqParams) extends Bundle {
  val baseAddr = UInt(p.addrWidth.W)
  val byteCount = UInt(32.W)
}
class ReactorDMAIO[A <: Data, B <: Data](c: ReactorDMAConfig, genIn: A, genOut: B)  extends Bundle {
  val memPort = new GenericMemoryMasterPort(c.mrp)

  val portRead = Seq.tabulate(c.nOutputPorts)(i => Flipped(new PortOutIO(PortIOConfig(c.nElemsOut(i)),genOut)))
  val portWrite = Seq.tabulate(c.nInputPorts)(i => Flipped(new PortInIO(PortIOConfig(c.nElemsIn(i)),genIn)))


  val portWrite = Flipped(new PortInIO(PortIOConfig(c.nElemsIn), genIn))


  val readStart = Flipped(Decoupled(new DmaTransactionInfo(c.mrp)))
  val readDone = Output(Bool())

  val writeStart = Flipped(Decoupled(new DmaTransactionInfo(c.mrp)))
  val writeDone = Output(Bool())

  def tieOffExt: Unit = {
    writeStart.valid  := false.B
    writeStart.bits := (new DmaTransactionInfo(c.mrp)).Lit(
      _.byteCount -> 0.U,
      _.baseAddr -> 0.U
    )
    readStart.valid := false.B
    readStart.bits := (new DmaTransactionInfo(c.mrp)).Lit(
      _.byteCount -> 0.U,
      _.baseAddr -> 0.U
    )

  }
}

class ReactorDMA[A <: Data, B <: Data](c: ReactorDMAConfig, genIn: A, genOut: B) extends Module {
  // TODO: Move this check to a better place
  val inWidth = genIn.getWidth
  val outWidth = genOut.getWidth
  require(inWidth < c.mrp.dataWidth, "[ReactorDMA.scala] The width of each input element must be smaller than that of the memory port")
  require(outWidth < c.mrp.dataWidth, "[ReactorDMA.scala] The width of each input element must be smaller than that of the memory port")


  val io = IO(new ReactorDMAIO(c,genIn, genOut))

  // Create the Memory StreamReader and StreamWriter
  // TODO: Currently streamWidth=dataWidth(64bit). I.e. this means that we dont PACK memory like we could
  //  In future release we probably wanna pack memory and store e.g. 8 x uint8_t per 64bit word
  val streamWriterParams = new StreamWriterParams(
    streamWidth = c.mrp.dataWidth, mem = c.mrp, chanID = 0, maxBeats = 1
  )
  val memStreamWriter = Module(new StreamWriter(streamWriterParams))

  val fifoElems = 4
  val streamReaderParams = new StreamReaderParams(
    streamWidth = c.mrp.dataWidth, fifoElems = fifoElems, mem = c.mrp, maxBeats = 1, chanID = 0,
    disableThrottle = true, readOrderCache = false, readOrderTxns = 4, streamName = "ReactorDMAStream", useChiselQueue = true
  )

  val memStreamReader = Module(new StreamReader(streamReaderParams))

  // Create the PortStreamReader and Writer
  val portStreamReaderConfig = PortIOConfig(nElems = c.nElemsOut)
  val portStreamReader = Module(new PortStreamReader(portStreamReaderConfig, genIn))

  val portStreamWriterConfig = PortIOConfig(nElems = c.nElemsIn)
  val portStreamWriter = Module(new PortStreamWriter(portStreamWriterConfig, genOut))

  // Unfortunatly streamReader and streamWriter depend on io.start staying high
  //  for the entire execution. This is achieved by these registers
  //  I would prefer a ready/valid interface
  val sIdle :: sRunning :: sWaiting :: sDone :: Nil = Enum(4)
  val regReadState = RegInit(sIdle)
  val regWriteState = RegInit(sIdle)

  val regWrBaseAddr = RegInit(0.U(c.mrp.addrWidth.W))
  val regWrByteCount = RegInit(0.U(32.W))

  val regRdBaseAddr = RegInit(0.U(c.mrp.addrWidth.W))
  val regRdByteCount = RegInit(0.U(32.W))



  // Connect MemStreamReader/Writer to the memory port
  memStreamReader.io.rsp <> io.memPort.memRdRsp
  memStreamReader.io.req <> io.memPort.memRdReq
  memStreamWriter.io.req <> io.memPort.memWrReq
  memStreamWriter.io.rsp <> io.memPort.memWrRsp
  memStreamWriter.io.wdat <> io.memPort.memWrDat

  // Connect PortStreamWriter/Reader to Port ports
  portStreamReader.io.portRead <> io.portRead
  portStreamWriter.io.portWrite <> io.portWrite

  // Connect PortReader to StreamWriter
  portStreamReader.io.out <> memStreamWriter.io.in

  // Connect PortWriter to StreamReader
  portStreamWriter.io.in <> memStreamReader.io.out

  // Writing FSM
  // Defaults
  io.writeStart.ready := false.B
  io.writeDone := false.B
  memStreamWriter.io.byteCount := 0.U
  memStreamWriter.io.baseAddr := 0.U
  memStreamWriter.io.start := false.B
  portStreamReader.io.start.valid := false.B

  switch (regWriteState) {
    is (sIdle) {
      io.writeStart.ready := true.B
      when(io.writeStart.fire) {
        regWriteState := sRunning
        regWrByteCount := io.writeStart.bits.byteCount
        regWrBaseAddr := io.writeStart.bits.baseAddr
        portStreamReader.io.start.valid := true.B
        assert(portStreamReader.io.start.fire, "[ReactorDMA] portStreamReader did not fire")

      }
    }

    is (sRunning) {
      memStreamWriter.io.byteCount := regWrByteCount
      memStreamWriter.io.baseAddr := regWrBaseAddr
      memStreamWriter.io.start := true.B

      when (memStreamWriter.io.finished) {
        regWriteState := sDone
      }
    }

    is (sDone) {
      io.writeDone := true.B
      regWriteState := sIdle
    }
  }

  // Reading FSM
  io.readDone := false.B
  io.readStart.ready := false.B
  memStreamReader.io.baseAddr := 0.U
  memStreamReader.io.byteCount := 0.U
  memStreamReader.io.initCount := 0.U
  memStreamReader.io.doInit := false.B
  memStreamReader.io.start := false.B

  switch (regReadState) {
    is (sIdle) {
      io.readStart.ready := true.B
      when(io.readStart.fire) {
        regReadState := sRunning
        regRdByteCount := io.readStart.bits.byteCount
        regRdBaseAddr := io.readStart.bits.baseAddr
      }
    }

    is (sRunning) {
      memStreamReader.io.baseAddr := regRdBaseAddr
      memStreamReader.io.byteCount := regRdByteCount
      memStreamReader.io.initCount := 0.U
      memStreamReader.io.doInit := false.B
      memStreamReader.io.start := true.B

      when (memStreamReader.io.finished) {
        regReadState := sWaiting
      }
    }

    is (sWaiting) {
      when (portStreamWriter.io.done) {
        regReadState := sDone
      }
    }

    is (sDone) {
      io.readDone := true.B
      regReadState := sIdle
    }
  }

  assert(!(io.readStart.fire && RegNext(memStreamReader.io.active)), "[ReactorDMA] DMA read was requested while already performing read")
  assert(!(io.writeStart.fire && RegNext(memStreamWriter.io.active)), "[ReactorDMA] DMA write was requested while already performing a write")
}


class ReactorDMAWithMem[A <: Data, B <: Data](c: ReactorDMAConfig, genIn: A, genOut: B) extends Module {
  val io = IO(new Bundle {
    val portRead = Flipped(new PortOutIO(PortIOConfig(c.nElemsOut),genOut))
    val portWrite = Flipped(new PortInIO(PortIOConfig(c.nElemsIn), genIn))


    val readStart = Flipped(Decoupled(new DmaTransactionInfo(c.mrp)))
    val readDone = Output(Bool())

    val writeStart = Flipped(Decoupled(new DmaTransactionInfo(c.mrp)))
    val writeDone = Output(Bool())
  })

  val ioMem = IO(new TestMemAccessIO(c.mrp))


  val testMem = Module(new TesterMemoryWrapper(c.mrp, 1))
  val reactorDma = Module(new ReactorDMA(c, genIn, genOut))
  reactorDma.io.memPort <> testMem.accio.memPort(0)

  io.portRead <> reactorDma.io.portRead
  io.portWrite <> reactorDma.io.portWrite
  io.readStart <> reactorDma.io.readStart
  io.writeStart <> reactorDma.io.writeStart
  io.readDone := reactorDma.io.readDone
  io.writeDone := reactorDma.io.writeDone

  ioMem <> testMem.verio


}
