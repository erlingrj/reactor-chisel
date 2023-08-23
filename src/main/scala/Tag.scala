package reactor
import chisel3._

object Tag {
  def FOREVER: UInt = {
    Time.FOREVER.ticks.U(width.W)
  }

  def NEVER: UInt = {
    Time.NEVER.ticks.S(width.W).asUInt
  }

  def apply(): UInt = UInt(width.W)

  def apply(initialValue: Long): UInt = {
    initialValue.U(width.W)
  }

  def apply(initialValue: UInt): UInt = {
    initialValue
  }

  def apply(initialValue: Time): UInt  = {
    initialValue.ticks.U
  }

  def width: Int = 64
}
