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
    val signal = Wire(new Signal(gen))
    signal.value := init
    signal
  }

}

