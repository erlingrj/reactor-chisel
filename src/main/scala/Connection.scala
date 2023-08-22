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

abstract class ConnectionIO[T1 <: Data, T2 <: Token[T1]](c: ConnectionConfig[T1,T2]) extends Bundle {
  val write: EventWriteSlave[T1,T2]
  val reads: Vec[EventReadSlave[T1,T2]]

  def driveDefaults(): Unit = {
    write.driveDefaults()
    reads.foreach(_.driveDefaults())
  }
}

class ConnectionSingleValueIO[T1 <: Data](c: ConnectionConfig[T1,SingleToken[T1]]) extends ConnectionIO(c) {
  val write = new EventSingleValueWriteSlave(c.genData)
  val reads = Vec(c.nChans, new EventSingleValueReadSlave(c.genData))
}

class ConnectionPureIO(c: ConnectionConfig[UInt, PureToken]) extends ConnectionIO(c) {
  val write = new EventPureWriteSlave
  val reads = Vec(c.nChans, new EventPureReadSlave)
}

class ConnectionArrayIO[T1 <: Data](c: ConnectionConfig[T1,ArrayToken[T1]]) extends ConnectionIO(c) {
  val write = new EventArrayWriteSlave(c.genData, c.genToken)
  val reads = Vec(c.nChans, new EventArrayReadSlave(c.genData, c.genToken))
}

abstract class Connection[T1 <: Data, T2 <: Token[T1]](c: ConnectionConfig[T1, T2]) extends Module {
  val io: ConnectionIO[T1,T2]
  // If there are no channels (i.e. no reactions triggered by this connection), then we still need
  // a channel for the writer to write into. An optimization would be to not do any writing in this case.
  var numReadChannels = c.nChans
  if (numReadChannels == 0) {
    println("WARNING: Got a connection with no triggered reactions downstream")
    numReadChannels = 1
  }

  val sIdle :: sToken :: Nil = Enum(2)
  val regState = RegInit(sIdle)

  val regTokens = RegInit(VecInit(Seq.fill(numReadChannels)(false.B)))
  val regTag = RegInit(0.U(64.W))
  val done = WireDefault(false.B)

  def main() = {
    io.driveDefaults()
    switch(regState) {
      is(sIdle) {
        io.write.ready := true.B
        when(io.write.req.valid) {
          regTag := io.write.req.token.tag
        }

        when(io.write.fire) {
          regState := sToken
          // Note that we use c.nChans here and not numReadChannels. In case c.nChans == 0
          for (i <- 0 until c.nChans) {
            regTokens(i) := true.B
          }
        }
      }

      is(sToken) {
        for (i <- 0 until c.nChans) {
          io.reads(i).resp.valid := regTokens(i)
          io.reads(i).resp.token.tag := regTag
          when(io.reads(i).fire) {
            regTokens(i) := false.B
          }
        }

        when(!regTokens.reduceTree(_ || _)) {
          regState := sIdle
          done := true.B
        }
      }
    }
  }
}

// A pure connection should just be used by Timers and Precedence ports. It does not support multiple writers
// The fire and valid and present signals must be asserted the same
class PureConnection(c : ConnectionConfig[UInt,PureToken]) extends Connection(c) {
  val io = IO(new ConnectionPureIO(c))
  main()

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
  val io = IO(new ConnectionSingleValueIO(c))
  main()

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

class ArrayConnection[T <: Data](c: ConnectionConfig[T, ArrayToken[T]]) extends Connection(c) {
  val io = IO(new ConnectionArrayIO(c))
  main()

  val data = for (i <- 0 until c.nChans) yield SyncReadMem(c.genToken.depth, c.genData)
  val present = RegInit(false.B)
  val tag = RegInit(Tag(0))

  switch(regState) {
    is(sIdle) {
      when(io.write.req.valid && io.write.req.present) {
        data.foreach(_.write(io.write.req.addr, io.write.req.token.data))
        tag := io.write.req.token.tag
        present := true.B
      }
    }
    is(sToken) {
      for (i <- 0 until c.nChans) {
        io.reads(i).resp.present := present && !done
        io.reads(i).resp.token.data := data(i).read(io.reads(i).asTypeOf(new EventArrayReadSlave(c.genData, c.genToken)).req.addr)
        io.reads(i).resp.token.tag := tag
      }

      when(done) {
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

class ArrayConnectionFactory[T1 <: Data](genData: T1, genToken: ArrayToken[T1]) extends ConnectionFactory(
  genData,genToken,
  genFunc = (c: ConnectionConfig[T1, ArrayToken[T1]]) => new ArrayConnection(c)
) {}