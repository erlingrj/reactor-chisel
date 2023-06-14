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
    tag.time := ~0.U
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
}
