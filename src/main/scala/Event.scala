package tiss

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
class ArrayToken[T <: Data](gen:T, val depth: Int) extends Token(gen) {
  val data = gen
  val valid = Bool()

  def addrWidth = Math.max(1,log2Ceil(depth))
}
class FifoToken[T <: Data](gen:T, val depth: Int) extends Token(gen) {
  val data = gen
  val valid = Bool()
}

// Create a separate class hierarchy for SwTokens, i.e. tokens coming from Software reactors
abstract class SwToken[T <: Data](gen: T) extends Bundle {
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
  def driveDefaults() = {}
}

class EventArrayReadReq[T1 <: Data](gen1: T1, gen2: ArrayToken[T1]) extends EventReadReq(gen1,gen2) {
  val addr = UInt(gen2.addrWidth.W)
  override def driveDefaults(): Unit = {
    super.driveDefaults()
    addr := 0.U
  }
}

class TokenWrReq[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  val valid = Bool()
  val present = Bool()
  val token = gen2

  def driveDefaults(): Unit = {
    valid := false.B
    present := false.B
    token := 0.U.asTypeOf(gen2)
  }
}

class EventArrayWriteReq[T1 <: Data](gen1: T1, gen2: ArrayToken[T1]) extends TokenWrReq(gen1,gen2) {
  val addr = UInt(gen2.addrWidth.W)
  override def driveDefaults(): Unit = {
    super.driveDefaults()
    addr := 0.U
  }
}

class TokenRdResp[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  val valid = Bool()
  val present = Bool()
  val token = gen2


  def driveDefaults(): Unit = {
    valid := false.B
    present := false.B
    token := 0.U.asTypeOf(gen2)
  }
}


abstract class EventInterface[T1,T2] extends Bundle {
}

abstract class TokenReader[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends EventInterface[T1,T2] {
  def genData = gen1
  def genToken = gen2

}

abstract class TokenReadMaster[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends TokenReader(gen1, gen2) {
  val req: EventReadReq[T1, T2]
  val resp: TokenRdResp[T1,T2]
  val fire = Output(Bool())

  def driveDefaultsFlipped(): Unit = {
    resp.driveDefaults()
  }

  def driveDefaults(): Unit = {
    req.driveDefaults()
    fire := false.B
  }
  def read: T1
  def read(addr: UInt): T1
  def read(addr: Int): T1
}

class ArrayTokenReadMaster[T1 <: Data](gen1: T1, gen2: ArrayToken[T1]) extends TokenReadMaster(gen1, gen2) {
  val req = Output(new EventArrayReadReq(gen1, gen2))
  val resp = Input(new TokenRdResp(gen1, gen2))

  def read: T1 = {
    require(false)
    0.U.asTypeOf(gen1)
  }

  // FIXME: Does this work?
  def read(addr: UInt) = {
    req.addr := addr
    WireInit(resp.token.data)
  }

  def read(addr: Int) = {
    req.addr := addr.U
    WireInit(resp.token.data)
  }
}

class SingleTokenReadMaster[T1 <: Data](gen1: T1) extends TokenReadMaster(gen1, new SingleToken(gen1)) {
  val req = Output(new EventReadReq(gen1, new SingleToken(gen1)))
  val resp = Input(new TokenRdResp(gen1, new SingleToken(gen1)))

  def read = resp.token.data

  def read(addr: UInt): T1 = {
    require(false)
    0.U.asTypeOf(gen1)
  }
  def read(addr: Int): T1 = {
    require(false)
    0.U.asTypeOf(gen1)
  }
}

class PureTokenReadMaster extends TokenReadMaster(UInt(0.W), new PureToken) {
  val req = Output(new EventReadReq(UInt(0.W), new PureToken))
  val resp = Input(new TokenRdResp(UInt(0.W), new PureToken))

  def read: UInt = {
    require(false)
    0.U
  }

  def read(addr: UInt): UInt = read
  def read(addr: Int): UInt = read
}

abstract class TokenReadSlave[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends TokenReader(gen1, gen2) {
  val req: EventReadReq[T1, T2]
  val resp: TokenRdResp[T1,T2]
  val fire = Input(Bool())

  def driveDefaultsFlipped(): Unit = {
    fire := false.B
    req.driveDefaults()
  }

  def driveDefaults(): Unit = {
    resp.driveDefaults()
  }
}

class SingleTokenReadSlave[T1 <: Data](gen1: T1) extends TokenReadSlave(gen1, new SingleToken(gen1)) {
  val req = Input(new EventReadReq(gen1, new SingleToken(gen1)))
  val resp = Output(new TokenRdResp(gen1, new SingleToken(gen1)))
}

class PureTokenReadSlave extends TokenReadSlave(UInt(0.W), new PureToken) {
  val req = Input(new EventReadReq(UInt(0.W), new PureToken))
  val resp = Output(new TokenRdResp(UInt(0.W), new PureToken))
}

class ArrayTokenReadSlave[T1 <: Data](gen1: T1, gen2: ArrayToken[T1]) extends TokenReadSlave(gen1, gen2) {
  val req = Input(new EventArrayReadReq(gen1, gen2))
  val resp = Output(new TokenRdResp(gen1, gen2))
}

class TokenWriter[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2:T2) extends EventInterface[T1, T2] {
  def genData = gen1
  def genToken = gen2
}

abstract class TokenWriteMaster[T1 <: Data, T2 <: Token[T1]] (genData: T1, genToken: T2) extends TokenWriter(genData,genToken) {
  val req: TokenWrReq[T1, T2]
  val ready = Input(Bool())
  val fire = Output(Bool())

  def write(d: T1): Unit

  def write(d: T1, addr: UInt): Unit

  def driveDefaultsFlipped(): Unit = {
    ready := true.B
  }

  def driveDefaults(): Unit = {
    fire := false.B
    req.driveDefaults()
  }

  def writeAbsent(): Unit = {
    assert(ready)
    req.valid := true.B
    req.present := false.B
    req.token := DontCare
  }

  def writeAbsentAndFire(): Unit = {
    writeAbsent()
    fire := true.B
  }

  def write(d: T1, addr: Int): Unit = {
    write(d,addr.U)
  }
}

class SingleTokenWriteMaster[T1 <: Data] (genData: T1) extends TokenWriteMaster(genData, new SingleToken(genData)) {
  val req = Output(new TokenWrReq(genData,new SingleToken(genData)))

  def write(d: T1): Unit = {
    assert(ready, "[Event] Tried writing to a port which was not ready")
    req.valid := true.B
    req.present := true.B
    req.token.data := d
  }
  def write(d: T1, addr: UInt): Unit = require(false)
}

class PureTokenWriteMaster extends TokenWriteMaster(UInt(0.W), new PureToken) {
  val req = Output(new TokenWrReq(UInt(0.W), new PureToken))

  def write(d: UInt): Unit = require(false)
  def write(d: UInt, addr: UInt): Unit = require(false)
}

class ArrayTokenWriteMaster[T1 <: Data] (genData: T1, genToken: ArrayToken[T1]) extends TokenWriteMaster(genData, genToken) {
  val req = Output(new EventArrayWriteReq(genData,genToken))

  def write(d: T1, addr: UInt): Unit = {
    assert(ready, "[Event] Tried writing to a port which was not ready")
    req.valid := true.B
    req.addr := addr
    req.present := true.B
    req.token.data := d
  }

  def write(d: T1): Unit = require(false)
}

abstract class TokenWriteSlave[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends TokenWriter(genData, genToken) {
  val req: TokenWrReq[T1,T2]
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

class SingleTokenWriteSlave[T1 <: Data](genData: T1) extends TokenWriteSlave(genData, new SingleToken(genData)) {
  val req = Input(new TokenWrReq(genData, new SingleToken(genData)))
}

class PureTokenWriteSlave extends TokenWriteSlave(UInt(0.W), new PureToken) {
  val req = Input(new TokenWrReq(UInt(0.W), new PureToken))
}

class ArrayTokenWriteSlave[T1 <: Data](genData: T1, genToken: ArrayToken[T1]) extends TokenWriteSlave(genData, genToken) {
  val req = Input(new EventArrayWriteReq(genData, genToken))
}

/**
 * A simple queue for buffering events coming out of the TriggerGenerator
 */
class EventWriteQueueIO[T1<: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val enq = genToken match {
    case genToken1: SingleToken[T1] => new SingleTokenWriteSlave(genData)
    case _ => new ArrayTokenWriteSlave(genData, genToken.asInstanceOf[ArrayToken[T1]])
  }

  val deq = genToken match {
    case genToken1: SingleToken[T1] => new SingleTokenWriteMaster(genData)
    case _ => new ArrayTokenWriteMaster(genData, genToken.asInstanceOf[ArrayToken[T1]])
  }
}

class EventWriteQueue[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2, nEntries: Int = 2) extends Module {
  val io = IO(new EventWriteQueueIO(genData, genToken))
  val q = Module(new Queue(new TokenWrReq(genData, genToken), nEntries))

  q.io.enq.bits := io.enq.req
  q.io.enq.valid := io.enq.fire
  io.enq.ready := q.io.enq.ready

  io.deq.req := q.io.deq.bits
  io.deq.fire := q.io.deq.valid
  q.io.deq.ready := io.deq.ready
}
