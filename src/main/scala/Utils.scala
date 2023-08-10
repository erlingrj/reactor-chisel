package reactor.util

import chisel3._
import chisel3.util._

import Numeric._
import scala.collection.mutable.ArrayBuffer


object ReactorFault extends ChiselEnum {
  val None, ReactionTimeout, MemoryFault = Value
}
