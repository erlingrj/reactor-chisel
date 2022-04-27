package reactor

import chisel3._
import chisel3.util._


/**
 * A tag is a timestamp. It is represented as a bundle of a UInt of size which is dependent on the parameters
 *
 */

class Tag(implicit rc: ReactorGlobalParams) extends Bundle {
  val tag = UInt(rc.numClkBits.W)
}

object Tag {
  def apply()(implicit rc: ReactorGlobalParams): Tag = {
    val tag = WireInit(new Tag)
    tag
  }

  def apply(init: UInt)(implicit rc: ReactorGlobalParams): Tag = {
    val tag = WireInit(new Tag)
    tag.tag := init
    tag
  }
}

