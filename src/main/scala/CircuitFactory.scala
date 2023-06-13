package reactor

import chisel3._


// This is an abstract class defining the factory classes. A factory object is a Scala object which you can interact
// with over multiple steps until you finally call `construct()` at which point a Chisel circuit is
abstract class CircuitFactory {
  // All factory classes must implement a construct method which returns a Seq of the newbuilt Modules.
  def construct(): Seq[Module]

}
