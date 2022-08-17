package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror

import fpgatidbits.PlatformWrapper._



abstract class ReactorBase(p: PlatformWrapperParams)(implicit val rp: ReactorGlobalParams) extends GenericAccelerator(p) {
  val numMemPorts = 1

  val io = IO(new GenericAcceleratorIF(numMemPorts, p) {
    val start = Input(Bool())
    val baseAddr = Input(Bool())
    val baseAddrRes = Input(Bool())
    val faultType = Output(ReactorFault())
    val faultReaction = Output(UInt(16.W))
    val cycles = Output(UInt(32.W))
    val currReaction = Output(UInt(8.W))
  })
  io.signature := makeDefaultSignature()

  val scheduler: Scheduler
  val reactions: Seq[Reaction]
  val ports:Seq[Port[_<:Data]]

}

class TestReactor(p: PlatformWrapperParams)(implicit val rp: ReactorGlobalParams) extends ReactorBase(p) {

}