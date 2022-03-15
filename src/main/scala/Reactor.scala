package reactor

import chisel3._
import chisel3.util._



object ReactorType extends Enumeration {
  type ReactorType = Value
  val Action, Input, Output, Reaction, Reactor = Value
}

abstract class ReactorElement(val _name: String, val _type: ReactorType.ReactorType) extends Module {

}


class Reactor(override val _name: String) extends ReactorElement(_name, ReactorType.Reactor) {

}

