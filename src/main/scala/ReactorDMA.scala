package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import fpgatidbits.dma._
import fpgatidbits.PlatformWrapper._

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil


case class ReactorDMAConfig(
  inPorts : Array[PortIOConfig[Data]],
  outPorts : Array[PortIOConfig[Data]],
  mrp : MemReqParams,
) {
  require( inPorts.length > 0 && inPorts.length < 256)
  require( outPorts.length > 0 && outPorts.length < 256)
  require( inPorts.map(_.gen.getWidth <= mrp.dataWidth).reduce(_&&_), "All top-level ports must have width less than system data width")
  require( outPorts.map(_.gen.getWidth <= mrp.dataWidth).reduce(_&&_), "All top-level ports must have width less than system data width")


  def maxInWidth: Int = {
    var max = 0
    for (p <- inPorts) {
      if (p.gen.getWidth > max) max = p.gen.getWidth
    }
    max
  }

  def nInPorts: Int = inPorts.length
  def nOutPorts: Int = outPorts.length

  // Calculate the byteCounts needed for each Input Port data fetch from DRAM
  def inByteCounts: Vec[UInt]= VecInit(Seq.tabulate(nInPorts)(i => (inPorts(i).nElems * 8).U(32.W)))

  // Calculate the base address for each Input Port data fetch from DRAM
  def inBaseAddrs(baseAddr: UInt): Vec[UInt] = {
    var offsets = ArrayBuffer[Int]()
    for (i <- 0 until nInPorts) {
      if (i == 0) {
        offsets += 0
      } else
        offsets += offsets(i-1) + inPorts(i-1).nElems*8
    }
    VecInit(Seq.tabulate(nInPorts)(i => baseAddr + offsets(i).U(mrp.addrWidth.W)))
  }


}

class DmaTransactionInfo(len: Int, p: MemReqParams) extends Bundle {
  val baseAddr = UInt(p.addrWidth.W)
  val present = Vec(len, Bool())
}
class ReactorDMAIO(c: ReactorDMAConfig)  extends Bundle {
  val memPort = new GenericMemoryMasterPort(c.mrp)

  val portRead = MixedVec(Seq.tabulate(c.inPorts.length)(i => Flipped(new PortOutIO(c.inPorts(i)))))
  val portWrite = MixedVec(Seq.tabulate(c.outPorts.length)(i => Flipped(new PortInIO(c.outPorts(i)))))

  val readStart = Flipped(Decoupled(new DmaTransactionInfo(c.nInPorts, c.mrp)))
  val readDone = Output(Bool())

  val writeStart = Flipped(Decoupled(new DmaTransactionInfo(c.nOutPorts, c.mrp)))
  val writeDone = Output(Bool())

  def tieOff: Unit = {
    portRead.map(p => {
      p.en := false.B
      p.addr := false.B
    })
    portWrite.map(p => {
      p.data := 0.U
      p.en := false.B
      p.addr := 0.U
    })
  }

  def tieOffExt: Unit = {
    writeStart.valid  := false.B
    writeStart.bits.baseAddr := 0.U
    writeStart.bits.present.foreach(_ := false.B)
    readStart.valid := false.B
    readStart.bits.baseAddr := 0.U
    readStart.bits.present.foreach(_ := false.B)
  }
}

class ReactorDMA(c: ReactorDMAConfig) extends Module {
  val io = IO(new ReactorDMAIO(c))
  io.tieOff

  // Create the Memory StreamReader and StreamWriter
  // TODO: Currently streamWidth=dataWidth(64bit). I.e. this means that we dont PACK memory like we could
  //  In future release we probably wanna pack memory and store e.g. 8 x uint8_t per 64bit word
  val streamWriterParams = new StreamWriterParams(
    streamWidth = c.mrp.dataWidth, mem = c.mrp, chanID = 0, maxBeats = 1
  )
  val memStreamWriter = Module(new StreamWriter(streamWriterParams)).io

  val fifoElems = 4
  val streamReaderParams = new StreamReaderParams(
    streamWidth = c.mrp.dataWidth, fifoElems = fifoElems, mem = c.mrp, maxBeats = 1, chanID = 0,
    disableThrottle = true, readOrderCache = false, readOrderTxns = 4, streamName = "ReactorDMAStream", useChiselQueue = true
  )

  val memStreamReader = Module(new StreamReader(streamReaderParams)).io

  // Create the PortStreamReader and Writer
  val portStreamReaders = for (outCfg <- c.outPorts) yield {
    Module(new PortStreamReader(outCfg)).io
  }
  portStreamReaders.map(_.tieOffExt())


  val _portStreamWriters = for (inCfg <- c.inPorts) yield {
    Module(new PortStreamWriter(inCfg)).io
  }
  // Create wires to connect to these Writers that are defined by the biggest width of a portWriter
  //  There are some problems created by the fact that different portWriters have different data widths
  //  These are solved by connecting them all to fixed-size wires which in turn are connected, one-by-one
  //  to the memStreamReader
  val portStreamWritersIn = Wire(Vec(c.nInPorts, Decoupled(UInt(c.maxInWidth.W))))
  val portStreamWritersDone = Wire(Vec(c.nInPorts, Bool()))
  for (i <- 0 until c.nInPorts) {
    portStreamWritersIn(i) <> _portStreamWriters(i).in
    portStreamWritersDone(i) := _portStreamWriters(i).done
    // Also drive default value of wires
    portStreamWritersIn(i).bits := 0.U
    portStreamWritersIn(i).valid := false.B
  }

  // Unfortunatly streamReader and streamWriter depend on io.start staying high
  //  for the entire execution. This is achieved by these registers
  //  I would prefer a ready/valid interface
  val sIdle :: sRunning :: sWaiting :: sDone :: Nil = Enum(4)
  val regReadState = RegInit(sIdle)
  val regWriteState = RegInit(sIdle)

  val regWrBaseAddr = RegInit(0.U(c.mrp.addrWidth.W))
  val regWrByteCount = RegInit(0.U(32.W))

  val regRdTxInfo = RegInit(0.U.asTypeOf(new DmaTransactionInfo(c.nInPorts, c.mrp)))
  val regRdByteCount = RegInit(0.U(32.W))
  val regRdTxCnt = RegInit(0.U(log2Ceil(c.nInPorts).W))



  // Connect MemStreamReader/Writer to the memory port
  memStreamReader.rsp <> io.memPort.memRdRsp
  memStreamReader.req <> io.memPort.memRdReq
  memStreamWriter.req <> io.memPort.memWrReq
  memStreamWriter.rsp <> io.memPort.memWrRsp
  memStreamWriter.wdat <> io.memPort.memWrDat

  // Connect PortStreamWriter/Reader to Port ports
  (portStreamReaders zip io.portRead).foreach {case(stream, port) => stream.portRead <> port}
  (_portStreamWriters zip io.portWrite).foreach {case(stream, port) => stream.portWrite <> port}

  // Reading FSM
  io.readDone := false.B
  io.readStart.ready := false.B
  memStreamReader.baseAddr := 0.U
  memStreamReader.byteCount := 0.U
  memStreamReader.initCount := 0.U
  memStreamReader.doInit := false.B
  memStreamReader.start := false.B

  // Connect memStreamReader and portStreamReader based on the loop iterator
  memStreamReader.out <> portStreamWritersIn(regRdTxCnt)

  // Reading FSM
  // Start in Idle and goes to Running->Waiting->Running until all data is read from shared mem
  // The portWriters are reconnected automatically through the MUX above and the counter used
  // to loop through the input ports
  switch (regReadState) {
    is (sIdle) {
      io.readStart.ready := true.B
      when(io.readStart.fire) {
        regReadState := sRunning
        regRdTxInfo := io.readStart.bits
        regRdTxCnt := 0.U
      }
    }

    is (sRunning) {
      when (regRdTxInfo.present(regRdTxCnt)) {
        memStreamReader.baseAddr := c.inBaseAddrs(regRdTxInfo.baseAddr)(regRdTxCnt)
        memStreamReader.byteCount := c.inByteCounts(regRdTxCnt)
        memStreamReader.initCount := 0.U
        memStreamReader.doInit := false.B
        memStreamReader.start := true.B
      } otherwise {
        when (regRdTxCnt === (c.nInPorts-1).U) {
          regReadState := sDone
        } otherwise {
          regRdTxCnt := regRdTxCnt + 1.U
        }
      }

      when (memStreamReader.finished) {
        assert(regRdTxInfo.present(regRdTxCnt))
        regReadState := sWaiting
      }
    }

    is (sWaiting) {
      when (portStreamWritersDone(regRdTxCnt)) {
        when (regRdTxCnt === (c.nInPorts-1).U) {
          regReadState := sDone
        } otherwise {
          regReadState := sRunning
          regRdTxCnt := regRdTxCnt + 1.U
        }
      }
    }

    is (sDone) {
      io.readDone := true.B
      regRdTxCnt := 0.U
      regReadState := sIdle
    }
  }

  // Writing FSM
  // Defaults
  io.writeStart.ready := false.B
  io.writeDone := false.B
  memStreamWriter.byteCount := 0.U
  memStreamWriter.baseAddr := 0.U
  memStreamWriter.start := false.B
  memStreamWriter.in.bits :=0.U
  memStreamWriter.in.valid := false.B


  assert(!(io.readStart.fire && RegNext(memStreamReader.active)), "[ReactorDMA] DMA read was requested while already performing read")
  assert(!(io.writeStart.fire && RegNext(memStreamWriter.active)), "[ReactorDMA] DMA write was requested while already performing a write")
}


class ReactorDMAWithMem(c: ReactorDMAConfig) extends Module {
  val io = IO(new Bundle {
    val portRead = MixedVec(Seq.tabulate(c.inPorts.length)(i => Flipped(new PortOutIO(c.inPorts(i)))))
    val portWrite = MixedVec(Seq.tabulate(c.outPorts.length)(i => Flipped(new PortInIO(c.outPorts(i)))))

    val readStart = Flipped(Decoupled(new DmaTransactionInfo(c.nInPorts, c.mrp)))
    val readDone = Output(Bool())

    val writeStart = Flipped(Decoupled(new DmaTransactionInfo(c.nOutPorts, c.mrp)))
    val writeDone = Output(Bool())
  })

  val ioMem = IO(new TestMemAccessIO(c.mrp))


  val testMem = Module(new TesterMemoryWrapper(c.mrp, 1))
  val reactorDma = Module(new ReactorDMA(c))
  reactorDma.io.memPort <> testMem.accio.memPort(0)

  io.portRead <> reactorDma.io.portRead
  io.portWrite <> reactorDma.io.portWrite
  io.readStart <> reactorDma.io.readStart
  io.writeStart <> reactorDma.io.writeStart
  io.readDone := reactorDma.io.readDone
  io.writeDone := reactorDma.io.writeDone

  ioMem <> testMem.verio


}
