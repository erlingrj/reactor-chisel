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
  val write: TokenWriteSlave[T1,T2]
  val reads: Vec[TokenReadSlave[T1,T2]]

  def driveDefaults(): Unit = {
    write.driveDefaults()
    reads.foreach(_.driveDefaults())
  }
}

class ConnectionSingleValueIO[T1 <: Data](c: ConnectionConfig[T1,SingleToken[T1]]) extends ConnectionIO(c) {
  val write = new SingleTokenWriteSlave(c.genData)
  val reads = Vec(c.nChans, new SingleTokenReadSlave(c.genData))
}

class ConnectionPureIO(c: ConnectionConfig[UInt, PureToken]) extends ConnectionIO(c) {
  val write = new PureTokenWriteSlave
  val reads = Vec(c.nChans, new PureTokenReadSlave)
}

class ConnectionArrayIO[T <: Data](c: ConnectionConfig[T,ArrayToken[T]]) extends ConnectionIO(c) {
  val write = new ArrayTokenWriteSlave(c.genData, c.genToken)
  val reads = Vec(c.nChans, new ArrayTokenReadSlave(c.genData, c.genToken))
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

  val regTokens = RegInit(VecInit(Seq.fill(numReadChannels)(false.B))) // Presence of token (including absent tokens)
  val regTag = RegInit(0.U(64.W))
  val done = WireDefault(false.B)

  def main() = {
    io.driveDefaults()
    switch(regState) {
      is(sIdle) {
        // Idling, waiting for the writing side of the connection to fire
        io.write.req.ready := true.B

        when(io.write.fire) {
          regTag := io.write.tag
          regState := sToken
          // Note that we use c.nChans here and not numReadChannels. In case c.nChans == 0
          for (i <- 0 until c.nChans) {
            regTokens(i) := true.B
          }
        }
      }

      is(sToken) {
        for (i <- 0 until c.nChans) {
          io.reads(i).req.ready := regTokens(i)
          io.reads(i).token := regTokens(i)
          io.reads(i).tag := regTag
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
       assert(io.write.absent || io.write.req.valid)
       regPresent.foreach(_ := io.write.req.valid)
     }
    }
    is (sToken) {
      for (i <- 0 until c.nChans) {
        when (io.reads(i).fire) {
          regPresent(i) := false.B
        }
        io.reads(i).present := regPresent(i)
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
      io.write.dat.ready := true.B
      when (io.write.dat.fire) {
        data := io.write.dat.bits.data
        present := true.B
      }
    }
    is (sToken) {
      for (i <- 0 until c.nChans) {
        io.reads(i).present := present && !done && regTokens(i)
        io.reads(i).resp.bits.data := data
        io.reads(i).resp.valid := present && !done && regTokens(i)
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

  val regIsWriting = RegInit(false.B)
  val regWrAddr = RegInit(0.U(c.genToken.addrWidth.W))
  val regWrSize = RegInit(0.U((c.genToken.sizeWidth).W))
  val regWrCnt = RegInit(0.U((c.genToken.sizeWidth).W))

//  printf("addr %d cnt %d size %d\n", regWrAddr, regWrCnt, regWrSize)
  switch(regState) {
    is(sIdle) {
      when(!regIsWriting) {
        io.write.req.ready := true.B
        io.write.dat.ready := false.B
        when(io.write.req.fire) {
          present := true.B
          regWrAddr := io.write.req.bits.addr
          regWrSize := io.write.req.bits.size
          regWrCnt := 0.U
          regIsWriting := true.B
        }
      }.otherwise {
        io.write.dat.ready := true.B
        val doneWriting = WireDefault(false.B)
        when(io.write.dat.fire) {
          data.foreach(_.write(regWrAddr, io.write.dat.bits.data))
          regWrAddr := regWrAddr + 1.U
          regWrCnt := regWrCnt + 1.U
          when(regWrCnt === regWrSize - 1.U) {
            regIsWriting := false.B
            doneWriting := true.B
          }
        }
        assert(!(io.write.fire && !doneWriting), "[Connection] Fired before all writes requested were performed")
      }
    }
    is(sToken) {
      for (i <- 0 until c.nChans) {
        io.reads(i).present := present && !done
        val regIsReading = RegInit(false.B)
        val regMemRespValid = RegInit(false.B)
        val regRdAddr = RegInit(0.U(c.genToken.addrWidth.W))
        val regRdSize = RegInit(0.U((c.genToken.sizeWidth.W))) // + 1 since we have to
        val regRdCnt = RegInit(0.U((c.genToken.sizeWidth).W))
        val doneReading = WireDefault(false.B)
        when(!regIsReading) {
          io.reads(i).req.ready := true.B
          io.reads(i).resp.valid := false.B
          when(io.reads(i).req.fire) {
            regIsReading := true.B
            regMemRespValid := false.B
            regRdAddr := io.reads(i).req.bits.asTypeOf(new ArrayTokenRdReq(c.genData, c.genToken)).addr
            regRdSize := io.reads(i).req.bits.asTypeOf(new ArrayTokenRdReq(c.genData, c.genToken)).size
            regRdCnt := 0.U
          }
        }.otherwise {
          val readResp = data(i).read(regRdAddr)
          regMemRespValid := true.B

          io.reads(i).resp.valid := regMemRespValid
          io.reads(i).resp.bits.data := readResp

          when(io.reads(i).resp.fire || !regMemRespValid) {
            regRdAddr := regRdAddr + 1.U
            regRdCnt := regRdCnt + 1.U
            when(regRdCnt === regRdSize) {
              regIsReading := false.B
              doneReading := true.B
            }
          }
          assert(!(io.reads(i).fire && !doneReading), "[Connection] Fired before all reads requested were accepted")
        }
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
  var upstream: TokenWriter[T1,T2] = null
  var downstreams: ArrayBuffer[Seq[TokenReader[T1,T2]]] = ArrayBuffer()
  def addUpstream(up: TokenWriter[T1,T2]): Unit = {
    require(upstream == null, "addUpstream called twice")
    upstream = up
  }
  def addDownstream(down: Seq[TokenReader[T1,T2]]): Unit = {
    downstreams += down
  }
  def >>(down: TokenReader[T1,T2]): Unit = {
    addDownstream(Seq(down))
  }

  def >>(down: Seq[TokenReader[T1, T2]]): Unit = {
    addDownstream(down)
  }


  def <<(up: TokenWriter[T1, T2]): Unit = {
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

    println(upstream)
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

class ArrayConnectionFactory[T1 <: Data](genData: T1, genToken: ArrayToken[T1]) extends ConnectionFactory[T1, ArrayToken[T1], ArrayConnection[T1]](
  genData,genToken,
  genFunc = (c: ConnectionConfig[T1, ArrayToken[T1]]) => new ArrayConnection(c)
) {}