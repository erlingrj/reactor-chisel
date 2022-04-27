package reactor

import chisel3._
import chisel3.util._


/**
 * A signal is what carries information between the Reactors
 *  A signal can be empty, tagged or untagged
 */


abstract class BaseSignal extends Bundle {

}

class Signal[T <: Data] extends BaseSignal {
  val value = new T
}

object Signal {
  def apply[T <: Data](implicit rc: ReactorGlobalParams): Signal[T] = {
    val signal = WireInit(new Signal[T])
    signal
  }

  def apply[T <: Data](init: T)(implicit rc: ReactorGlobalParams): Signal[T] = {
    val signal = WireInit(new Signal[T])
    signal.value := init
    signal
  }

}


class TaggedSignal[T <: Data](implicit rc: ReactorGlobalParams) extends Signal[T] {
  val tag = Tag()
}


object TaggedSignal {
  def apply[T <: Data](implicit rc: ReactorGlobalParams): TaggedSignal[T] = {
    val signal = WireInit(new TaggedSignal[T])
    signal
  }

  def apply[T <: Data](initVal: T, initTag: UInt)(implicit rc: ReactorGlobalParams): TaggedSignal[T] = {
    val signal = WireInit(new TaggedSignal[T])
    signal.tag := initTag
    signal.value := initVal
    signal
  }

}



class EmptySignal extends BaseSignal {

}
