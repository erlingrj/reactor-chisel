package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import scala.collection.mutable.ArrayBuffer
case class ConnectionConfig[T1 <: Data, T2 <: Token[T1]]
(
  genData: T1,
  genToken: T2,
  nChans: Int
)

class ConnectionIO[T1 <: Data, T2 <: Token[T1]](c: ConnectionConfig[T1,T2]) extends Bundle {
  val write = new EventWriteSlave(c.genData, c.genToken)
  val reads = Vec(c.nChans, new EventReadSlave(c.genData, c.genToken))

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
  val regTag = RegInit(0.U(64.W))
  val done = WireDefault(false.B)

  switch(regState) {
    is (sIdle) {
      io.write.ready := true.B
      when (io.write.req.valid) {
        regTag := io.write.req.token.tag
      }
      when (io.write.fire) {
        regState := sToken
        regTokens.foreach(_ := true.B)
      }
    }

    is (sToken) {
      for (i <- 0 until c.nChans) {
        io.reads(i).resp.valid := regTokens(i)
        io.reads(i).resp.token.tag := regTag
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

// A pure connection should just be used by Timers and Precedence ports. It does not support multiple writers
// The fire and valid and present signals must be asserted the same
class PureConnection(c : ConnectionConfig[UInt,PureToken]) extends Connection(c) {
  val regPresent = RegInit(VecInit(Seq.fill(c.nChans)(false.B)))
  switch (regState) {
    is (sIdle) {
     when (io.write.fire) {
       regPresent.foreach(_ := io.write.req.present)
     }
    }
    is (sToken) {
      for (i <- 0 until c.nChans) {
        when (io.reads(i).fire) {
          regPresent(i) := false.B
        }
        io.reads(i).resp.present := regPresent(i)
      }
    }
  }
}

class SingleValueConnection[T <: Data](c: ConnectionConfig[T, SingleToken[T]]) extends Connection(c) {
  val data = RegInit(0.U.asTypeOf(c.genData))
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
        data := 0.U.asTypeOf(c.genData)
        present := false.B
      }
    }
  }
}

// The `genFunc` is what complicates this.
class ConnectionFactory[T1 <: Data, T2 <: Token[T1], T3 <: Connection[T1, T2]](
                                                                              genData: T1,
                                                                              genToken: T2,
                                                                genFunc: ConnectionConfig[T1,T2] => T3,
                                                                ) extends CircuitFactory {
  var upstream: EventWriter[T1,T2] = null
  var downstreams: ArrayBuffer[Seq[EventReader[T1,T2]]] = ArrayBuffer()
  def addUpstream(up: EventWriter[T1,T2]): Unit = {
    require(upstream == null, "addUpstream called twice")
    upstream = up
  }
  def addDownstream(down: Seq[EventReader[T1,T2]]): Unit = {
    downstreams += down
  }
  def >>(down: EventReader[T1,T2]): Unit = {
    addDownstream(Seq(down))
  }

  def >>(down: Seq[EventReader[T1, T2]]): Unit = {
    addDownstream(down)
  }


  def <<(up: EventWriter[T1, T2]): Unit = {
    addUpstream(up)
  }


  def construct(): Seq[T3] = {
    require(upstream != null, "Connection.construct() called without anything connecting to the upstream port")
    require(downstreams.nonEmpty, "Connection.construct() called with nothing connected to the downstream port")
    val config = ConnectionConfig(
      genData = genData,
      genToken = genToken,
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
    Seq(conn)
  }
}

class SingleValueConnectionFactory[T1 <: Data](genData: T1) extends ConnectionFactory(
  genData,
  new SingleToken(genData),
  genFunc = (c: ConnectionConfig[T1, SingleToken[T1]]) => new SingleValueConnection(c)
) {}

// Convenience class to generate the PureConnections more easy
class PureConnectionFactory() extends ConnectionFactory(
  UInt(0.W),
  new PureToken,
  genFunc = {(c: ConnectionConfig[UInt, PureToken]) => new PureConnection(c)}
) {}