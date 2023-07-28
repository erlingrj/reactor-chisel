package reactor

import chisel3._

class Tag extends Bundle {
  val time = UInt(64.W)
  def +(other: UInt): Tag =Tag( this.time + other)
  def +(other: Tag): Tag = Tag(this.time + other.time)

}

object Tag {
  def FOREVER: Tag = {
    val tag = Wire(new Tag())
    tag.time := Time.FOREVER.ticks.U(64.W)
    tag
  }

  def NEVER: Tag = {
    val tag = Wire(new Tag())
    tag.time := Time.NEVER.ticks.U(64.W)
    tag
  }

  def apply(initialValue: Long): Tag = {
    val tag = Wire(new Tag())
    tag.time := initialValue.U
    tag
  }

  def apply(initialValue: UInt): Tag = {
    val tag = Wire(new Tag())
    tag.time := initialValue
    tag
  }

  def apply(initialValue: Time): Tag = {
    val tag = Wire(new Tag())
    tag.time := initialValue.ticks
    tag
  }

  def width: Int = 64
}
