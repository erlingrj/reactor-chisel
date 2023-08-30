package reactor

import chisel3._
import chisel3.util._
import reactor.Tag

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
class ArrayToken[T <: Data](gen:T, val depth: Long) extends Token(gen) {
  val data = gen

  def addrWidth = Math.max(1,log2Ceil(depth))
  def sizeWidth = addrWidth+1
  def bytesPerToken = gen.getWidth/8
}
class FifoToken[T <: Data](gen:T, val depth: Long) extends Token(gen) {
  val data = gen
}

// Create a separate class hierarchy for SwTokens, i.e. tokens coming from Software reactors
// Due to ArrayTokens ALWAYS having address as INPUT, we saw the need to split SwToken into Input and Output since we
// cant just do Input(ArrayToken).
abstract class SwToken[T <: Data](gen: T) extends Bundle {
  val present: Bool
}
class SwSingleTokenInput[T <: Data](gen: T) extends SwToken(gen) {
  val present = Input(Bool())
  val data = Input(gen)
}
class SwSingleTokenOutput[T <: Data](gen: T) extends SwToken(gen) {
  val present = Output(Bool())
  val data = Output(gen)
}

class SwArrayTokenInput[T <: Data](gen: T) extends SwToken(gen) {
  val present = Input(Bool())
  val addr = Input(UInt(32.W)) // FIXME: This assumes 32bit shared memory space
  def data = gen
}
class SwArrayTokenOutput[T <: Data](gen: T) extends SwToken(gen) {
  val present = Output(Bool())
  val addr = Input(UInt(32.W)) // FIXME: This assumes 32bit shared memory space
  def data = gen
}

class TokenRdReq[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  def driveDefaults() = {}
}

class ArrayTokenRdReq[T1 <: Data](gen1: T1, gen2: ArrayToken[T1]) extends TokenRdReq(gen1,gen2) {
  val addr = UInt(gen2.addrWidth.W)
  val size = UInt(gen2.sizeWidth.W)
  override def driveDefaults(): Unit = {
    super.driveDefaults()
    addr := 0.U
    size := 0.U
  }
}

class TokenWrReq[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends Bundle {
  def driveDefaults() = {}
}

class TokenWrDat[T1 <: Data](gen1: T1) extends Bundle {
  val data = gen1
  def driveDefaults() = {
    data := 0.U.asTypeOf(gen1)
  }
}

class ArrayTokenWrReq[T1 <: Data](gen1: T1, gen2: ArrayToken[T1]) extends TokenWrReq(gen1,gen2) {
  val addr = UInt(gen2.addrWidth.W)
  val size = UInt(gen2.sizeWidth.W)
  override def driveDefaults(): Unit = {
    super.driveDefaults()
    addr := 0.U
    size := 0.U
  }
}

class TokenRdResp[T1 <: Data](gen1: T1) extends Bundle {
  val data = gen1

  def driveDefaults(): Unit = {
    data := 0.U.asTypeOf(gen1)
  }
}


abstract class TokenInterface[T1,T2] extends Bundle {
}

abstract class TokenReader[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends TokenInterface[T1,T2] {
  def genData = gen1
  def genToken = gen2

}

abstract class TokenReadMaster[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends TokenReader(gen1, gen2) {
  val req: DecoupledIO[TokenRdReq[T1, T2]]
  val resp: DecoupledIO[TokenRdResp[T1]]
  val fire = Output(Bool())
  val tag = Input(Tag())
  val token = Input(Bool())
  val present = Input(Bool())

  def driveDefaultsFlipped(): Unit = {
    tag := 0.U
    token := false.B
    present := false.B
    req.nodeq()
    resp.noenq()

  }

  def driveDefaults(): Unit = {
    req.noenq()
    resp.nodeq()
    fire := false.B
  }

  def read: DecoupledIO[TokenRdResp[T1]]
  def read(addr: UInt, size: UInt): DecoupledIO[TokenRdResp[T1]]
}

class ArrayTokenReadMaster[T1 <: Data](gen1: T1, gen2: ArrayToken[T1]) extends TokenReadMaster(gen1, gen2) {
  val req = Decoupled(new ArrayTokenRdReq(gen1, gen2))
  val resp = Flipped(Decoupled(new TokenRdResp(gen1)))

  def read: DecoupledIO[TokenRdResp[T1]] = {
    require(false)
    resp
  }

  def read(addr: UInt, size: UInt): DecoupledIO[TokenRdResp[T1]] = {
    req.valid := true.B
    req.bits.addr := addr
    req.bits.size := size
    assert(req.fire)
    resp
  }
}

class SingleTokenReadMaster[T1 <: Data](gen1: T1) extends TokenReadMaster(gen1, new SingleToken(gen1)) {
  val req = Decoupled(new TokenRdReq(gen1, new SingleToken(gen1)))
  val resp = Flipped(Decoupled(new TokenRdResp(gen1)))

  def read = resp

  def read(addr: UInt, size: UInt) = {
    require(false)
    resp
  }
}

class PureTokenReadMaster extends TokenReadMaster(UInt(0.W), new PureToken) {
  val req = Decoupled(new TokenRdReq(UInt(0.W), new PureToken))
  val resp = Flipped(Decoupled(new TokenRdResp(UInt(0.W))))

  def read = {
    require(false)
    resp
  }

  def read(addr: UInt, size: UInt) = read

}

abstract class TokenReadSlave[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2: T2) extends TokenReader(gen1, gen2) {
  val req: DecoupledIO[TokenRdReq[T1, T2]]
  val resp: DecoupledIO[TokenRdResp[T1]]
  val fire = Input(Bool())
  val token = Output(Bool())
  val present = Output(Bool())
  val tag = Output(Tag())

  def driveDefaultsFlipped(): Unit = {
    fire := false.B
    req.noenq()
    resp.nodeq()
  }

  def driveDefaults(): Unit = {
    token := false.B
    present := false.B
    tag := 0.U
    resp.noenq()
    req.nodeq()
  }
}

class SingleTokenReadSlave[T1 <: Data](gen1: T1) extends TokenReadSlave(gen1, new SingleToken(gen1)) {
  val req = Flipped(Decoupled(new TokenRdReq(gen1, new SingleToken(gen1))))
  val resp = Decoupled(new TokenRdResp(gen1))
}

class PureTokenReadSlave extends TokenReadSlave(UInt(0.W), new PureToken) {
  val req = Flipped(Decoupled(new TokenRdReq(UInt(0.W), new PureToken)))
  val resp = Decoupled(new TokenRdResp(UInt(0.W)))
}

class ArrayTokenReadSlave[T1 <: Data](gen1: T1, gen2: ArrayToken[T1]) extends TokenReadSlave(gen1, gen2) {
  val req = Flipped(Decoupled(new ArrayTokenRdReq(gen1, gen2)))
  val resp = Decoupled(new TokenRdResp(gen1))
}

class TokenWriter[T1 <: Data, T2 <: Token[T1]](gen1: T1, gen2:T2) extends TokenInterface[T1, T2] {
  def genData = gen1
  def genToken = gen2
}

/**
 * @req: A write request with address and size. The ready signal here is used to indicate whether the other side is
 * ready to accept any request
 * @dat: The data accompanying a write request.
 * @fire: Signal that the token is finished and can be forwarded
 * @tag: The timestamp of the token. Its value should be latched when fire is high
 * @absent: If high with fire, this means that we are firing off an absent token.
 *
 * Firing semantics:
 *
 * fire && req.valid => Firing off a PresentToken (Probably a Pure present token)
 * fire && absent => Firing off an AbsentToken
 * fire && !req.valid && !absent => firing off a token. It is either absent or present depending on whether anything
 *  was written to it.
 */
abstract class TokenWriteMaster[T1 <: Data, T2 <: Token[T1]] (genData: T1, genToken: T2) extends TokenWriter(genData,genToken) {
  val req: DecoupledIO[TokenWrReq[T1, T2]]
  val dat: DecoupledIO[TokenWrDat[T1]]
  val fire = Output(Bool())
  val tag = Output(Tag())
  val absent = Output(Bool())

  def writePresent(): Unit = {
    fire := true.B
    req.valid := true.B
    absent := false.B
  }

  def writeAbsent(): Unit = {
    fire := true.B
    absent := true.B
    req.valid := false.B
  }

  def driveDefaultsFlipped(): Unit = {
    req.nodeq()
    dat.nodeq()
  }

  // To "plug" a port, accept its data and drop it
  def plugUnusedFromOutside(): Unit = {
    req.deq()
    dat.deq()
  }

  def driveDefaults(): Unit = {
    fire := false.B
    tag := Tag(0)
    absent := false.B
    req.noenq()
    dat.noenq()
  }
}

class SingleTokenWriteMaster[T1 <: Data] (genData: T1) extends TokenWriteMaster(genData, new SingleToken(genData)) {
  val req = Decoupled(new TokenWrReq(genData,new SingleToken(genData)))
  val dat = Decoupled(new TokenWrDat(genData))
}

class PureTokenWriteMaster extends TokenWriteMaster(UInt(0.W), new PureToken) {
  val req = Decoupled(new TokenWrReq(genData, new PureToken))
  val dat = Decoupled(new TokenWrDat(genData))
}

class ArrayTokenWriteMaster[T1 <: Data] (genData: T1, genToken: ArrayToken[T1]) extends TokenWriteMaster(genData, genToken) {
  val req = Decoupled(new ArrayTokenWrReq(genData,genToken))
  val dat = Decoupled(new TokenWrDat(genData))
}

// See TokenWriteMaster for signal description
abstract class TokenWriteSlave[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends TokenWriter(genData, genToken) {
  val req: DecoupledIO[TokenWrReq[T1, T2]]
  val dat: DecoupledIO[TokenWrDat[T1]]
  val fire = Input(Bool())
  val tag = Input(Tag())
  val absent = Input(Bool())

  def driveDefaultsFlipped(): Unit = {
    fire := false.B
    tag := Tag(0)
    absent := false.B
    req.noenq()
    dat.noenq()
  }

  def driveDefaults(): Unit = {
    req.nodeq()
    dat.nodeq()
  }

  def firedAbsent() = fire && absent
  def firedPresent() = fire && req.valid && !absent
  def firedHistory() = fire && !req.valid && !absent
}

class SingleTokenWriteSlave[T1 <: Data](genData: T1) extends TokenWriteSlave(genData, new SingleToken(genData)) {
  val req = Flipped(Decoupled(new TokenWrReq(genData, new SingleToken(genData))))
  val dat = Flipped(Decoupled(new TokenWrDat(genData)))
}

class PureTokenWriteSlave extends TokenWriteSlave(UInt(0.W), new PureToken) {
  val req = Flipped(Decoupled(new TokenWrReq(genData, new PureToken)))
  val dat = Flipped(Decoupled(new TokenWrDat(genData)))
}

class ArrayTokenWriteSlave[T1 <: Data](genData: T1, genToken: ArrayToken[T1]) extends TokenWriteSlave(genData, genToken) {
  val req = Flipped(Decoupled(new ArrayTokenWrReq(genData, genToken)))
  val dat = Flipped(Decoupled(new TokenWrDat(genData)))
}
