package reactor

import chisel3._


/**
 * A tag is a timestamp. It is represented as a bundle of a UInt of size which is dependent on the parameters
 *
 */

class TimeTag(implicit rc: ReactorGlobalParams) extends Bundle {
  val tag: UInt = UInt(rc.numClkBits.W)
}

object TimeTag {
  def apply()(implicit rc: ReactorGlobalParams): TimeTag = {
    val tag = new TimeTag()
    tag
  }

  def apply(init: UInt)(implicit rc: ReactorGlobalParams): TimeTag = {
    val tag = Wire(new TimeTag())
    tag.tag := init
    tag
  }
}

