package reactor

import chisel3._
import chisel3.util._
abstract class BaseEvent extends Bundle {
  val valid = Output(Bool())
  val present = Output(Bool())
  val fire = Input(Bool())
  def absent: Bool = !present
  def driveDefaults: Unit
  def driveDefaultsFlipped: Unit

}

class PureEvent extends BaseEvent {
  def driveDefaults = { valid := false.B
    present := false.B
  }
  def driveDefaultsFlipped = {
    fire := false.B
  }
}

class EventWritePort[T <: Data](gen: T, depth: Int) extends Bundle {
  val en = Input(Bool())
  val data = Input(gen)
  val addrBits = log2Ceil(depth)
  val addr = if (addrBits >= 1) Some(Input(UInt(addrBits.W))) else None
}
class EventReadPort[T <: Data](gen: T, depth: Int) extends Bundle {
  val data = Output(gen)
  val addrBits = log2Ceil(depth)
  val addr = if (addrBits >= 1) Some(Input(UInt(addrBits.W))) else None

  def driveDefaults = {
    data := 0.U.asTypeOf(gen)
  }

  def driveDefaultsFlipped = {
    if (addr.isDefined) {
      addr.get := 0.U
    }
  }
}

class Event[T <: Data](gen: T, depth: Int) extends BaseEvent {
  val read = new EventReadPort(gen, depth)

  def driveDefaults = {
    valid := false.B
    present := false.B
    read.driveDefaults
  }

  def driveDefaultsFlipped = {
    fire := false.B
    read.driveDefaultsFlipped
  }
}