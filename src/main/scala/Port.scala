package reactor

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._


abstract class ReactorPortIO[T <: Token] extends Bundle {

}
case class InputPortConfig[T <: Token] (
                                   gen: T,
                                   nReaders: Int
                                 ) {
  def nReadersWidth = if (nReaders == 1) 1.W else log2Ceil(nReaders).W
}
class InputPortIO[T <: Token](c: InputPortConfig[T]) extends ReactorPortIO[T] {
  val inward = Vec(c.nReaders, new EventReadSlave(c.gen))
  val outward = new EventReadMaster(c.gen)

  def driveDefaults(): Unit = {
    inward.foreach(_.driveDefaults())
    outward.driveDefaults()
  }

  def plugInwards(): Unit = {
    inward.foreach(i => {
      i.fire := false.B
    })
  }

}

class InputPort[T <: Token](c: InputPortConfig[T]) extends Module {
  val io = IO(new InputPortIO(c))
  assert(!(io.outward.fire && io.outward.resp.valid), "[Event.scala] Reader asserted `fire` while `valid` was false")
  io.driveDefaults()
  io.inward.foreach(_ <> io.outward)

  // If we only have a single reader, then we just pass through the signal.
  // If there are more readers we do a state machine to control when the eat
  // signal is sent.
  if (c.nReaders > 1) {
    io.outward.fire:= false.B

    val regCount = RegInit(0.U(c.nReadersWidth))
    for (i <- 0 until c.nReaders) {
      when(io.inward(i).fire) {
        regCount := regCount + 1.U
      }
    }
    when(regCount === c.nReaders.U) {
      io.outward.fire:= true.B
      regCount := 0.U
    }
  }

  var downstreamIdx = 0
  def connectDownstream(d: EventReadMaster[T]): Unit = {
    io.inward(downstreamIdx) <> d
    downstreamIdx += 1
  }

  var upstreamConnected = false
  def connectUpstream(up: EventReadMaster[T]) = {
    require(!upstreamConnected, "[Port.scala] connectUpstream called twice on InputPort")
    io.outward <> up
    upstreamConnected = true
  }
}

case class OutputPortConfig[T <: Token](
                                      gen: T,
                                      nWriters: Int
                                      ) {
  def nWritersWidth: Width = if (nWriters == 1) 1.W else log2Ceil(nWriters).W
}
class OutputPortIO[T <: Token](c: OutputPortConfig[T]) extends ReactorPortIO[T] {
  val inward = Vec(c.nWriters, new EventWriteSlave(c.gen))
  val outward = new EventWriteMaster(c.gen)

  def driveDefaults(): Unit = {
    inward.foreach(_.driveDefaults())
    outward.driveDefaults()
  }

  def plugInwards(): Unit = {
    inward.foreach(i => {
      i.fire := false.B
      i.req.valid := false.B
      i.req.token := 0.U.asTypeOf(i.req.token)
      i.req.present := false.B
    })
  }
}

class OutputPort[T <: Token](c: OutputPortConfig[T]) extends Module {
  val io = IO(new OutputPortIO(c))
  io.driveDefaults()


  val regCount = RegInit(0.U(c.nWritersWidth))

  for (i <- 0 until c.nWriters) {
    when (io.inward(i).req.valid) {
      io.inward(i) <> io.outward
      when (io.inward(i).fire) {
        regCount := regCount + 1.U
      }
    }
  }

  io.outward.fire := false.B
  when(regCount === c.nWriters.U) {
    io.outward.fire := true.B
    regCount := 0.U
  }

  var upstreamIdx = 0
  def connectUpstream(up: EventWriteMaster[T]) = {
    io.inward(upstreamIdx) <> up
    upstreamIdx += 1
  }

  var downstreamConnected = false
  def connectDownstream(down: EventWriteMaster[T]) = {
    require(!downstreamConnected)
    io.outward <> down
    downstreamConnected = true
  }
}