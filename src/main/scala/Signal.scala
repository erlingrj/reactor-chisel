package reactor

import chisel3._

/**
 * A signal is what carries information between the Reactors
 *  A signal can be empty, tagged or untagged
 */


abstract class BaseSignal extends Bundle {

}

class Signal[T <: Data](gen: T) extends BaseSignal {
  val value: T = gen
}

object Signal {
  def apply[T <: Data](gen: T)(implicit rc: ReactorGlobalParams): Signal[T] = {
    val signal = new Signal(gen)
    signal
  }

  def apply[T <: Data](gen: T, init: T)(implicit rc: ReactorGlobalParams): Signal[T] = {
    val signal = WireInit(new Signal(gen))
    signal.value := init
    signal
  }

}


class TaggedSignal[T <: Data](gen: T)(implicit rc: ReactorGlobalParams) extends Signal(gen) {
  val tag: TimeTag = TimeTag()
}


object TaggedSignal {
  def apply[T <: Data](gen: T)(implicit rc: ReactorGlobalParams): TaggedSignal[T] = {
    val signal = WireInit(new TaggedSignal(gen))
    signal
  }

  def apply[T <: Data](gen:T, initVal: T, initTag: UInt)(implicit rc: ReactorGlobalParams): TaggedSignal[T] = {
    val signal = WireInit(new TaggedSignal(gen))
    signal.tag := initTag
    signal.value := initVal
    signal
  }
}



class EmptySignal extends BaseSignal {
}
