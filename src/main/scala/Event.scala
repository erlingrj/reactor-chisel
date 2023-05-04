package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror.directionOf

// This file defined the Event interfaces
// Events are in reactor-chisel referring to the IO between Connections and Reactor-ports

abstract class Token[T <: Data](gen: T) extends Bundle {
}
class PureToken extends Token[UInt](UInt(0.W)) {
}

class SingleToken[T <: Data](gen: T) extends Token(gen) {
  val data = gen
}
class ArrayToken[T <: Data](gen:T, depth: Int) extends Token(gen) {
  val data = gen
  val valid = Bool()
}
class FifoToken[T <: Data](gen:T, depth: Int) extends Token(gen) {
  val data = gen
  val valid = Bool()
}

// Create a separate class hierarchy for SwTokens, i.e. tokens coming from Software reactors
abstract class SwToken[T <: Data](gen: T) extends Token(gen) {}
class SwSingleToken[T <: Data](gen: T) extends SwToken(gen) {
  val data = gen
  val present= Bool()
}

class SwArrayToken[T <: Data](gen: T) extends SwToken(gen) {
  val addr = UInt(32.W) // FIXME: This assumes 32bit shared memory space
  val size = UInt(32.W)
  val present = Bool()
}



class EventReadReq[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  gen1 match {
    case _: ArrayToken[_] =>
      require(false)
      val addr = UInt(8.W)
    case _: FifoToken[_] =>
      val ready = Bool()
    case _ =>
  }

  def driveDefaults(): Unit = {
    gen1 match {
      case _: ArrayToken[_] =>
        require(false)
      case _: FifoToken[_] =>
      case _ =>
    }
  }
}

class EventWriteReq[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  val valid = Bool()
  val present = Bool()
  val token = gen2

  def driveDefaults(): Unit = {
    if (directionOf(valid) == ActualDirection.Output) {
      valid := false.B
      present := false.B
      token := 0.U.asTypeOf(gen2)
    }
  }

}

class EventReadResp[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  val valid = Bool()
  val present = Bool()
  val token = gen2

  def driveDefaults(): Unit = {
    if (directionOf(valid) == ActualDirection.Output) {
      valid := false.B
      present := false.B
      token := 0.U.asTypeOf(gen2)
    }
  }
}

class EventReadMaster[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  val req = Output(new EventReadReq(gen1,gen2))
  val resp = Input(new EventReadResp(gen1,gen2))
  val fire = Output(Bool())

  def driveDefaults(): Unit = {
    req.driveDefaults()
    resp.driveDefaults()
    if (directionOf(fire) == ActualDirection.Output) {
      fire := false.B
    }
  }

  def read(): T1 = {
    gen2 match {
      case _: SingleToken[T1] =>
        resp.token.asInstanceOf[SingleToken[T1]].data
      case _ =>
        assert(false.B)
        0.U.asInstanceOf[T1]
    }
  }

  def read(addr: UInt): T1 = {
    gen2 match {
      case _: ArrayToken[T1] =>
        assert(false.B)
        0.U.asInstanceOf[T1]
      case _ =>
        assert(false.B)
        0.U.asInstanceOf[T1]
    }
  }
}

class EventReadSlave[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  val req = Input(new EventReadReq(gen1, gen2))
  val resp = Output(new EventReadResp(gen1, gen2))
  val fire = Input(Bool())

  def driveDefaults(): Unit = {
    req.driveDefaults()
    resp.driveDefaults()
    if (directionOf(fire) == ActualDirection.Output) {
      fire := false.B
    }
  }
}

class EventWriteMaster[T1 <: Data, T2 <: Token[T1]] (gen1: T1, gen2: T2) extends Bundle {
  val req = Output(new EventWriteReq(gen1,gen2))
  val fire = Output(Bool())
  def driveDefaults(): Unit = {
    if (directionOf(fire) == ActualDirection.Output) {
      fire := false.B
    }
    req.driveDefaults()
  }

  def write(d: T1): Unit = {
    req.valid := true.B
    req.present := true.B
    gen2 match {
      case _: SingleToken[T1] =>
        req.token.asInstanceOf[SingleToken[T1]].data := d
      case _ => assert(false.B)
    }
  }

  def write(d: T1, addr: UInt) = {
    req.valid := true.B
    req.present := true.B
    gen2 match {
      case _: ArrayToken[T1] => assert(false.B)
      case _ => assert(false.B)
    }
  }

}


class EventWriteSlave[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  val req = Input(new EventWriteReq(gen1, gen2))
  val fire = Input(Bool())
  def driveDefaults(): Unit = {
    if (directionOf(fire) == ActualDirection.Output) {
      fire := false.B
    }
    req.driveDefaults()
  }
}