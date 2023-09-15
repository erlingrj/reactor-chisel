package reactor
import chisel3._

object Tag {
  // FIXME: This requires 32 bit tag width. Is it a dangerous route?
  def FOREVER: UInt = {
    Long.MaxValue.U(width.W)
  }

  def NEVER: UInt = {
    Long.MinValue.S(width.W).asUInt
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
