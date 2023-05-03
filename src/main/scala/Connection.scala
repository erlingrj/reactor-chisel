package reactor

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer
case class ConnectionConfig[T <: Token]
(
  gen: T,
  nChans: Int
)
class ConnectionIO[T <: Token](c: ConnectionConfig[T]) extends Bundle {
  val write = new EventWriteSlave(c.gen)
  val reads = Vec(c.nChans, new EventReadSlave(c.gen))

  def driveDefaults(): Unit = {
    write.driveDefaults()
    reads.foreach(_.driveDefaults())
  }
}

abstract class Connection[T <: Token](c: ConnectionConfig[T]) extends Module {
  val io = IO(new ConnectionIO(c))
  io.driveDefaults()

  val sIdle :: sToken :: Nil = Enum(2)

  val regState = RegInit(sIdle)
  val regTokens = RegInit(VecInit(Seq.fill(c.nChans)(false.B)))
  val done = WireDefault(false.B)

  switch(regState) {
    is (sIdle) {
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

class PureConnection(c : ConnectionConfig[PureToken]) extends Connection(c) {

}
class SingleValueConnection[T <: Data](c: ConnectionConfig[SingleToken[T]]) extends Connection(c) {
  val data = RegInit(0.U.asTypeOf(c.gen.data))
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
        data := 0.U.asTypeOf(c.gen.data)
        present := false.B
      }
    }
  }
}

class ConnectionBuilder[T1 <: Token, T2 <: Connection[T1]](
                                                                genFunc: ConnectionConfig[T1] => T2,
                                                                genToken: T1
                                                                ) {
  var upstream: EventWriteMaster[T1] = null
  var downstreams: ArrayBuffer[Vec[EventReadMaster[T1]]] = ArrayBuffer()
  def addUpstream(up: EventWriteMaster[T1]): Unit = {
    require(upstream == null)
    upstream = up
  }
  def addDownstream(down: Vec[EventReadMaster[T1]]): Unit = {
    downstreams += down
  }

  def construct(): T2 = {
    val config = ConnectionConfig(
      gen = genToken,
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