package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror

import fpgatidbits.PlatformWrapper._


trait ReactorBaseIO extends Module {

  val ioUsr = IO(new Bundle  {
    val start = Input(Bool())
    val baseAddr = Input(Bool())
    val baseAddrRes = Input(Bool())
    val faultType = Output(ReactorFault())
    val faultReaction = Output(UInt(16.W))
    val cycles = Output(UInt(32.W))
    val currReaction = Output(UInt(8.W))
  })


}
abstract class ReactorBase(p: PlatformWrapperParams)(implicit val rp: ReactorGlobalParams)
  extends GenericAccelerator(p) with ReactorBaseIO {

  val numMemPorts = 1
  val ioMem = IO(new GenericAcceleratorMemIO(numMemPorts, p))
  val ioGen = IO(new GenericAcceleratorCSRIO(p))
  ioGen.signature := makeDefaultSignature()

}
