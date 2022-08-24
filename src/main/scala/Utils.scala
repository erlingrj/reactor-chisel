package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum


object ReactorFault extends ChiselEnum {
  val None, ReactionTimeout, MemoryFault = Value
}
