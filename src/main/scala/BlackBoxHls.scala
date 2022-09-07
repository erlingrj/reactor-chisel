package reactor

import chisel3._
import chisel3.util._


class VitisHlsInputPort(p: PortIOConfig[Data]) extends Bundle {
  val address0 = Output(UInt(p.nAddrBits.W))
  val ce0 = Output(Bool())
  val q0 = Input(UInt(p.nDataBits.W))
  val present = Input(Bool())
}

class VitisHlsOutputPort(p: PortIOConfig[Data]) extends Bundle {
  val address0 = Output(UInt(p.nAddrBits.W))
  val ce0 = Output(Bool())
  val we0 = Output(Bool())
  val d0 = Output(UInt(p.nDataBits.W))
}

class VitisHlsControlIO extends Bundle {
  val local_block = Output(Bool())
  val local_deadlock = Output(Bool())
  val clk = Input(Clock())
  val rst = Input(Bool())
  val start = Input(Bool())
  val done = Output(Bool())
  val idle = Output(Bool())
  val ready = Output(Bool())
}

abstract class BlackBoxVitisHls(c: ReactionConfig) extends BlackBox with HasBlackBoxResource {

}