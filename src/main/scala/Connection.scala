package reactor

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer
case class ConnectionConfig[T1 <: Data, T2 <: Token[T1]]
(
  gen1: T1,
  gen2: T2,
  nChans: Int
)
class ConnectionIO[T1 <: Data, T2 <: Token[T1]](c: ConnectionConfig[T1,T2]) extends Bundle {
  val write = new EventWriteSlave(c.gen1, c.gen2)
  val reads = Vec(c.nChans, new EventReadSlave(c.gen1, c.gen2))

  def driveDefaults(): Unit = {
    write.driveDefaults()
    reads.foreach(_.driveDefaults())
  }
}

abstract class Connection[T1 <: Data, T2 <: Token[T1]](c: ConnectionConfig[T1, T2]) extends Module {
  val io = IO(new ConnectionIO(c))
  io.driveDefaults()

  val sIdle :: sToken :: Nil = Enum(2)

  val regState = RegInit(sIdle)
  val regTokens = RegInit(VecInit(Seq.fill(c.nChans)(false.B)))
  val done = WireDefault(false.B)

  switch(regState) {
    is (sIdle) {
      io.write.ready := true.B
      when (io.write.fire) {
        regState := sToken
        regTokens.foreach(_ := true.B)
      }
    }

    is (sToken) {
      for (i <- 0 until c.nChans) {
        io.reads(i).resp.valid := regTokens(i)
        when(io.reads(i).fire) {
          regTokens(i) := false.B
        }
      }
      when (!regTokens.reduceTree(_ || _)) {
        regState := sIdle
        done := true.B
      }
    }
  }
}

class PureConnection(c : ConnectionConfig[UInt,PureToken]) extends Connection(c) {

  // In the PureConnection, there is no absent/present, so present is always high when we fire
  when (regState === sToken) {
    for (i <- 0 until c.nChans) {
      io.reads(i).resp.present := regTokens(i)
    }
  }
}
class SingleValueConnection[T <: Data](c: ConnectionConfig[T, SingleToken[T]]) extends Connection(c) {
  val data = RegInit(0.U.asTypeOf(c.gen1))
  val present = RegInit(false.B)

  switch(regState) {
    is (sIdle) {
      when (io.write.req.valid && io.write.req.present) {
        data := io.write.req.token.data
        present := true.B
      }
    }
    is (sToken) {
      for (i <- 0 until c.nChans) {
        io.reads(i).resp.present := present && !done
        io.reads(i).resp.token.data := data
      }

      when (done) {
        data := 0.U.asTypeOf(c.gen1)
        present := false.B
      }
    }
  }
}

class ConnectionBuilder[T1 <: Data, T2 <: Token[T1], T3 <: Connection[T1, T2]] (
                                                                genFunc: ConnectionConfig[T1,T2] => T3,
                                                                genData: T1,
                                                                genToken: T2
                                                                ) {
  var upstream: EventWriteMaster[T1,T2] = null
  var downstreams: ArrayBuffer[Seq[EventReadMaster[T1,T2]]] = ArrayBuffer()
  def addUpstream(up: EventWriteMaster[T1, T2]): Unit = {
    require(upstream == null)
    upstream = up
  }
  def addDownstream(down: Seq[EventReadMaster[T1,T2]]): Unit = {
    downstreams += down
  }
  def >>(down: EventReadMaster[T1,T2]): Unit = {
    addDownstream(Seq(down))
  }

  def >>(down: Vec[EventReadMaster[T1, T2]]): Unit = {
    addDownstream(down)
  }
  def <<(up: EventWriteMaster[T1, T2]): Unit = {
    addUpstream(up)
  }


  def construct(): T3 = {
    val config = ConnectionConfig(
      gen1 = genData,
      gen2 = genToken,
      nChans  = downstreams.map(_.length).sum
    )
    val conn = Module(genFunc(config))
    conn.io.write <> upstream
    var idx = 0
    for (i <- 0 until downstreams.length) {
      for (j <- 0 until downstreams(i).length) {
        conn.io.reads(idx) <> downstreams(i)(j)
        idx += 1
      }
    }
    conn
  }
}

// Convenience class to generate the PureConnections more easy
class PureConnectionBuilder extends ConnectionBuilder(
  genFunc = {(c: ConnectionConfig[UInt, PureToken]) => new PureConnection(c)},
  genData = UInt(0.W),
  genToken = new PureToken
) {
  def <<(up: Timer): Unit = {
    addUpstream(up.io.trigger)
  }
}