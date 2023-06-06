package reactor

import chisel3._
import chisel3.experimental.DataMirror.directionOf
import chisel3.util._


class StateReadReq[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  genToken match {
    case _: ArrayToken[_] =>
      require(false)
      val addr = UInt(8.W)
    case _: FifoToken[_] =>
      val ready = Bool()
    case _: SingleToken[_] =>
  }

  def driveDefaults() = {}
}

class StateReadResp[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val data = genData

  def driveDefaults(): Unit = {
    if (directionOf(data) == ActualDirection.Output) {
      data := 0.U.asTypeOf(genData)
    }
  }
}

class StateWriteReq[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val write = Bool()
  val data = genData

  genToken match {
    case _: ArrayToken[_] =>
      val addr = UInt(8.W)
      require(false)
    case _: FifoToken[_] =>
    case _: SingleToken[_] =>
  }

  def driveDefaults() = {
    if (directionOf(write) == ActualDirection.Output) {
      write := true.B
      data := 0.U.asTypeOf(genData)
    }
  }
}
class StateReadMaster[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val req = Output(new StateReadReq(genData, genToken))
  val resp = Input(new StateReadResp(genData, genToken))

  def driveDefaults() = {
    req.driveDefaults()
    resp.driveDefaults()
  }
}

class StateWriteMaster[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val req = Output(new StateWriteReq(genData, genToken))

  def driveDefaults() = {
    req.driveDefaults()
  }
}
class StateReadSlave[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val req = Input(new StateReadReq(genData, genToken))
  val resp = Output(new StateReadResp(genData, genToken))

  def driveDefaults() = {
    req.driveDefaults()
    resp.driveDefaults()
  }
}

class StateWriteSlave[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val req = Input(new StateWriteReq(genData, genToken))

  def driveDefaults() = {
    req.driveDefaults()
  }
}

class StateReadWriteMaster[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val read = new StateReadMaster(genData, genToken)
  val write = new StateWriteMaster(genData, genToken)
}

class StateReadWriteSlave[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val read = new StateReadSlave(genData, genToken)
  val write = new StateWriteSlave(genData, genToken)
}
case class StateConfig[T1 <: Data, T2 <: Token[T1]]
(
  genData: T1,
  genToken: T2,
  nReactions: Int
)
class StateIO[T1 <: Data, T2 <: Token[T1]](c: StateConfig[T1,T2]) extends Bundle {
  val ports = Vec(c.nReactions, new StateReadWriteSlave(c.genData, c.genToken))
}
abstract class State[T1 <: Data, T2 <: Token[T1]](c: StateConfig[T1, T2]) extends Module {
  val io = IO(new StateIO(c))


  var nReactions = 0
  def <> (reaction: StateReadWriteMaster[T1,T2]) = {
    require(c.nReactions > nReactions)
    io.ports(nReactions) <> reaction
    nReactions += 1
  }
}


class SingleValueState[T1 <: Data](c: StateConfig[T1, SingleToken[T1]]) extends State(c) {

  val data = RegInit(0.U.asTypeOf(c.genData))

  for (port <- io.ports) {
    port.read.resp := data
    when (port.write.req.write) {
      data := port.write.req.data
    }
  }

  assert(!(PopCount(io.ports.map(_.write.req.write)) > 1.U), "[State.scala] Multiple reactions tried writing to State at the same time")
}
