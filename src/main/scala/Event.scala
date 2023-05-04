package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror.directionOf

// This file defined the Event interfaces
// Events are in reactor-chisel referring to the IO between Connections and Reactor-ports

abstract class Token extends Bundle {
}
class PureToken extends Token {
}
class SingleToken[T <: Data](gen: T) extends Token {
  val data = gen
}

class ArrayToken[T <: Data](gen:T, depth: Int) extends Token {
  val data = gen
  val valid = Bool()
}
class FifoToken[T <: Data](gen:T, depth: Int) extends Token {
  val data = gen
  val valid = Bool()
}



// Create a separate class hierarchy for SwTokens, i.e. tokens coming from Software reactors
abstract class SwToken extends Token {}
class SwSingleToken[T <: Data](gen: T) extends SwToken{
  val data = gen
  val present= Bool()
}

class SwArrayToken extends SwToken{
  val addr = UInt(32.W) // FIXME: This assumes 32bit shared memory space
  val valid = Bool()
}



class EventReadReq[T <: Token](gen: T) extends Bundle {
  gen match {
    case _: ArrayToken[_] =>
      require(false)
      val addr = UInt(8.W)
    case _: FifoToken[_] =>
      val ready = Bool()
    case _ =>
  }

  def driveDefaults(): Unit = {
    gen match {
      case _: ArrayToken[_] =>
        require(false)
      case _: FifoToken[_] =>
      case _ =>
    }
  }
}

class EventWriteReq[T <: Token](gen: T) extends Bundle {
  val valid = Bool()
  val present = Bool()
  val token = gen

  def driveDefaults(): Unit = {
    if (directionOf(valid) == ActualDirection.Output) {
      valid := false.B
      present := false.B
      token := 0.U.asTypeOf(gen)
    }
  }

  def write[T2 <: Data] (d: T2): Unit = {
    valid := true.B
    present := true.B
    gen match {
      case _: SingleToken[T2] =>
        token.asInstanceOf[SingleToken[T2]].data := d
      case _ => assert(false.B)
    }
  }
  def writeAbsent() = {
    valid := true.B
    present := false.B
  }
}

class EventReadResp[T <: Token](gen: T) extends Bundle {
  val valid = Bool()
  val present = Bool()
  val token = gen

  def driveDefaults(): Unit = {
    if (directionOf(valid) == ActualDirection.Output) {
      valid := false.B
      present := false.B
      token := 0.U.asTypeOf(gen)
    }
  }
}

class EventReadMaster[T <: Token](gen: T) extends Bundle {
  val req = Output(new EventReadReq(gen))
  val resp = Input(new EventReadResp(gen))
  val fire = Output(Bool())

  def driveDefaults(): Unit = {
    req.driveDefaults()
    resp.driveDefaults()
    if (directionOf(fire) == ActualDirection.Output) {
      fire := false.B
    }
  }
}

class EventReadSlave[T <: Token](gen: T) extends Bundle {
  val req = Input(new EventReadReq(gen))
  val resp = Output(new EventReadResp(gen))
  val fire = Input(Bool())

  def driveDefaults(): Unit = {
    req.driveDefaults()
    resp.driveDefaults()
    if (directionOf(fire) == ActualDirection.Output) {
      fire := false.B
    }
  }
}

class EventWriteMaster[T <: Token](gen: T) extends Bundle {
  val req = Output(new EventWriteReq(gen))
  val fire = Output(Bool())
  def driveDefaults(): Unit = {
    if (directionOf(fire) == ActualDirection.Output) {
      fire := false.B
    }
    req.driveDefaults()
  }

}


class EventWriteSlave[T <: Token](gen: T) extends Bundle {
  val req = Input(new EventWriteReq(gen))
  val fire = Input(Bool())
  def driveDefaults(): Unit = {
    if (directionOf(fire) == ActualDirection.Output) {
      fire := false.B
    }
    req.driveDefaults()
  }
}