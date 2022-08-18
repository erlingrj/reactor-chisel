package reactor

import chisel3._
import chisel3.util._

import fpgatidbits.dma._
import fpgatidbits.PlatformWrapper._


case class ReactorDMAConfig(
  nElems : Int,
  mrp : MemReqParams
)
class ReactorDMAIO[A <: Data, B <: Data](genIn: A, genOut: B, c: ReactorDMAConfig) extends Bundle {
  val memPort = new GenericMemoryMasterPort(c.mrp)

}

class ReactorDMA[A <: Data, B <: Data](genIn: A, genOut: B, c: ReactorDMAConfig) extends Module {
  // TODO: Move this check to a better place
  val inWidth = genIn.getWidth
  val outWidth = genOut.getWidth
  require(inWidth < c.mrp.dataWidth, "[ReactorDMA.scala] The width of each input element must be smaller than that of the memory port")
  require(outWidth < c.mrp.dataWidth, "[ReactorDMA.scala] The width of each input element must be smaller than that of the memory port")

  val io = IO(new ReactorDMAIO(genIn, genOut,c))

  val streamWriterParams = new StreamWriterParams(
    streamWidth = outWidth, mem = c.mrp, chanID = 0, maxBeats = 1
  )
  val streamWriter = Module(new StreamWriter(streamWriterParams))

  val streamReaderParams = new StreamReaderParams(
    streamWidth = inWidth, fifoElems = 4, mem = c.mrp, maxBeats = 1, chanID = 0
  )
  val streamReader = new StreamReader(streamReaderParams)

  // Connect StreamReader/Writer to the memory port
  streamReader.io.rsp <> io.memPort.memRdRsp
  streamReader.io.req <> io.memPort.memRdReq
  streamWriter.io.req <> io.memPort.memWrReq
  streamWriter.io.rsp <> io.memPort.memWrRsp
  streamWriter.io.wdat <> io.memPort.memWrDat



}
