package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror.directionOf

// This file defined the Event interfaces
// Events are in reactor-chisel referring to the IO between Connections and Reactor-ports

// FIXME: Implement companion objects to avoid 'new' everywhere
abstract class Token[T <: Data](gen: T) extends Bundle {
  val tag = UInt(64.W) // FIXME: This should not be used alot, because it creates a huge mess.
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
abstract class SwToken[T <: Data](gen: T) extends Token(gen) {
  val present = Bool()
}
class SwSingleToken[T <: Data](gen: T) extends SwToken(gen) {
  val data = gen
}

class SwArrayToken[T <: Data](gen: T) extends SwToken(gen) {
  val addr = UInt(32.W) // FIXME: This assumes 32bit shared memory space
  val size = UInt(32.W)
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
    valid := false.B
    present := false.B
    token := 0.U.asTypeOf(gen2)
  }

}

class EventReadResp[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  val valid = Bool()
  val present = Bool()
  val token = gen2


  def driveDefaults(): Unit = {
    valid := false.B
    present := false.B
    token := 0.U.asTypeOf(gen2)
  }
}

class EventReader[T1 <: Data, T2 <: Token[T1]] extends Bundle {}
class EventReadMaster[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends EventReader[T1,T2] {
  val req = Output(new EventReadReq(gen1,gen2))
  val resp = Input(new EventReadResp(gen1,gen2))
  val fire = Output(Bool())

  def driveDefaultsFlipped(): Unit = {
    resp.driveDefaults()
  }

  def driveDefaults(): Unit = {
    req.driveDefaults()
    fire := false.B
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

class EventReadSlave[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends EventReader[T1, T2] {
  val req = Input(new EventReadReq(gen1, gen2))
  val resp = Output(new EventReadResp(gen1, gen2))
  val fire = Input(Bool())

  def driveDefaultsFlipped(): Unit = {
    fire := false.B
    req.driveDefaults()
  }

  def driveDefaults(): Unit = {
    resp.driveDefaults()
  }
}

class EventWriter[T1 <: Data, T2 <: Token[T1]] extends Bundle {}

class EventWriteMaster[T1 <: Data, T2 <: Token[T1]] (genData: T1, genToken: T2) extends EventWriter[T1,T2] {
  val req = Output(new EventWriteReq(genData,genToken))
  val ready = Input(Bool())
  val fire = Output(Bool())

  def driveDefaultsFlipped(): Unit = {
    ready := true.B
  }

  def driveDefaults(): Unit = {
    fire := false.B
    req.driveDefaults()
  }

  def write(d: T1): Unit = {
    assert(ready)
    req.valid := true.B
    req.present := true.B
    genToken match {
      case _: SingleToken[T1] =>
        req.token.asInstanceOf[SingleToken[T1]].data := d
      case _: PureToken => // OK
      case _ => assert(false.B)
    }
  }

  def writeAbsent(): Unit = {
    assert(ready)
    req.valid := true.B
    req.present := false.B
  }

  def write(d: T1, addr: UInt): Unit = {
    assert(ready)
    req.valid := true.B
    req.present := true.B
    genToken match {
      case _: ArrayToken[T1] => assert(false.B)
      case _ => assert(false.B)
    }
  }
}

class EventWriteSlave[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends EventWriter[T1,T2] {
  val req = Input(new EventWriteReq(genData, genToken))
  val ready = Output(Bool())
  val fire = Input(Bool())

  def driveDefaultsFlipped(): Unit = {
    fire := false.B
    req.driveDefaults()
  }

  def driveDefaults(): Unit = {
    ready := false.B
  }
}

/**
 * A simple queue for buffering events coming out of the TriggerGenerator
 */
class EventWriteQueueIO[T1<: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val enq = new EventWriteSlave(genData,genToken)
  val deq = new EventWriteMaster(genData, genToken)
}

class EventWriteQueue[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2, nEntries: Int = 2) extends Module {
  val io = IO(new EventWriteQueueIO(genData, genToken))
  val q = Module(new Queue(new EventWriteReq(genData, genToken), nEntries))

  q.io.enq.bits := io.enq.req
  q.io.enq.valid := io.enq.fire
  io.enq.ready := q.io.enq.ready

  io.deq.req := q.io.deq.bits
  io.deq.fire := q.io.deq.valid
  q.io.deq.ready := io.deq.ready
}
