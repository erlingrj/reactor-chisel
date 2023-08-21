package reactor

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

import scala.collection.mutable.ArrayBuffer


abstract class ReactorPortIO[T1 <: Data, T2 <: Token[T1]] extends Bundle {

}
case class InputPortConfig[T1 <: Data, T2 <: Token[T1]] (
                                   genData: T1,
                                   genToken : T2,
                                   nReaders: Int
                                 ) {
  def nReadersWidth = if (nReaders == 1) 1.W else log2Ceil(nReaders).W
}
class InputPortIO[T1 <: Data, T2 <: Token[T1]](c: InputPortConfig[T1,T2]) extends ReactorPortIO[T1,T2] {
  val inward = Vec(c.nReaders, new EventReadSlave(c.genData, c.genToken))
  val outward = new EventReadMaster(c.genData, c.genToken)

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

class InputPort[T1 <: Data, T2 <: Token[T1]](c: InputPortConfig[T1,T2]) extends Module {
  val io = IO(new InputPortIO(c))
  io.driveDefaults()
  io.inward.foreach(_ <> io.outward)

  // If we only have a single reader, then we just pass through the signal.
  // If there are more readers we do a state machine to control when the eat
  // signal is sent.
  if (c.nReaders > 1) {
    io.outward.fire:= false.B

    val regCount = RegInit(0.U(c.nReadersWidth+1.W))
    val regTokens = RegInit(VecInit(Seq.fill(c.nReaders)(false.B)))


    // New token from upstream
    when (regCount === 0.U && io.outward.resp.valid) {
      regTokens.foreach(_ := true.B)
    }

    for (i <- 0 until c.nReaders) {
      io.inward(i).resp.valid := regTokens(i)
      io.inward(i).resp.present := regTokens(i) && io.outward.resp.present
      when(io.inward(i).fire) {
        assert(regTokens(i), "[Port.scala] Reaction fired but port hadnt stored any tokens for it")
        regCount := regCount + 1.U
        regTokens(i) := false.B
      }
    }
    when(regCount === c.nReaders.U) {
      io.outward.fire:= true.B
      regCount := 0.U
    }
    assert(util.PopCount(io.inward.map(_.fire)) <= 1.U, "[Port.scala] Multiple inwards connected reactions fired simultanously")
  }

  var downstreamIdx = 0
  def connectDownstream(d: EventReadMaster[T1,T2]): Unit = {
    io.inward(downstreamIdx) <> d
    downstreamIdx += 1
  }

  var upstreamConnected = false
  def connectUpstream(up: EventReadMaster[T1, T2]) = {
    require(!upstreamConnected, "[Port.scala] connectUpstream called twice on InputPort")
    io.outward <> up
    upstreamConnected = true
  }

  def >>(down: EventReadMaster[T1, T2]): Unit  = {
    connectDownstream(down)
  }

  def >>(downs: Seq[EventReadMaster[T1, T2]]): Unit = {
    downs.foreach(connectDownstream(_))
  }

  def <<(up: EventReadMaster[T1,T2]): Unit = {
    connectUpstream(up)
  }
  def <<(up: Vec[EventReadMaster[T1, T2]]): Unit = {
    connectUpstream(up(0))
  }
}

case class OutputPortConfig[T1 <: Data, T2 <: Token[T1]](
                                      genData: T1,
                                      genToken: T2,
                                      nWriters: Int
                                      ) {
  def nWritersWidth: Width = if (nWriters == 1) 1.W else log2Ceil(nWriters).W
}
class OutputPortIO[T1 <: Data, T2 <: Token[T1]](c: OutputPortConfig[T1, T2]) extends ReactorPortIO[T1,T2] {
  val inward = Vec(c.nWriters, new EventWriteSlave(c.genData, c.genToken))
  val outward = new EventWriteMaster(c.genData, c.genToken)

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

class OutputPort[T1 <: Data, T2 <: Token[T1]](c: OutputPortConfig[T1, T2]) extends Module {
  val io = IO(new OutputPortIO(c))
  io.driveDefaults()

  if (c.nWriters == 1) {
    io.inward(0) <> io.outward
  } else {
    val regCount = RegInit(0.U(c.nWritersWidth + 1.W))

    io.inward(regCount) <> io.outward
    when(io.inward(regCount).fire) {
      regCount := regCount + 1.U
    }

    io.outward.fire := false.B
    when(regCount === c.nWriters.U) {
      io.outward.fire := true.B
      regCount := 0.U
    }
  }

  var upstreamIdx = 0
  def connectUpstream(up: EventWriteMaster[T1, T2]) = {
    io.inward(upstreamIdx) <> up
    upstreamIdx += 1
  }

  var downstreamConnected = false
  def connectDownstream(down: EventWriteMaster[T1, T2]) = {
    require(!downstreamConnected)
    io.outward <> down
    downstreamConnected = true
  }

  def <<(up: EventWriteMaster[T1,T2]) :Unit = {
    connectUpstream(up)
  }

  def <<(ups: Seq[EventWriteMaster[T1, T2]]): Unit = {
    ups.foreach(connectUpstream(_))
  }

  def >>(down: EventWriteMaster[T1, T2]): Unit = {
    connectDownstream(down)
  }

  assert(!(io.outward.fire && !io.outward.ready))
  assert(!(io.outward.req.valid && !io.outward.ready))
}

/**
 * This class can construct an Inward connected (IC) InputPort. A  IC InputPort is used to connect the *input* port of a reactor
 * to an *input* port of a contained child reactor. We use a builder because the *width* of these PT port is unknown from the
 * perspective of the parent reactor.
 *
 * It should be used as follows:
 * 1. Create it
 * 2. Connect it to the downstream contained reactors input port
 * 3. Now, use the `width` API to create the IO of the parent reactor
 * 4. use `declareInput` to connect the IO of the parent reactor
 * 5. `construct` to build the connection
 * @tparam T1
 *
 */
class InputPortInwardConnectionFactory[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends CircuitFactory {

  // Array of the downstream Input ports of contained reactors
  // An ArrayBuffer is used to "iteratively" grow this array
  var downstream: ArrayBuffer[EventReadMaster[T1,T2]] = ArrayBuffer()

  // A Seq of Input ports in the parent reactor. Here we use an immutable Seq. Since we only create this
  // once, at construction.
  var upstream: Seq[EventReadMaster[T1, T2]] = Seq()

  def nDownstreamInwards = downstream.length

  // Declare that a contained input port is downstream. Here the argument is a Seq of EventReadMaster (e.g. input ports)
  // This is because the contained input port might have multiple "channels" because it itself might have contained reactors
  // to which it forwards the connection.
  def declareDownstream(down: Seq[EventReadMaster[T1,T2]]) = {
    down.foreach(downstream += _)
  }
  // Convenient shorthand for declareDownstream
  def >>(down: Seq[EventReadMaster[T1,T2]]) = {
    declareDownstream(down)
  }

  // Declare the Input port in the parent reactor. Check the width.
  // Either it is equal the number of downstreams, or it is 1 greater (due to an internal port)
  def declareInput(up: Seq[EventReadMaster[T1, T2]]) = {
    if (up.length == nDownstreamInwards) upstream = up
    else if (up.length == (nDownstreamInwards + 1)) upstream = up.drop(1)
    else require(false)
  }
  def <<(up: Seq[EventReadMaster[T1, T2]]) = {
    declareInput(up)
  }

  // This constructs the actual h
  def construct(): Seq[Module] = {
    val config = InputPortConfig(genData = genData, genToken = genToken, nReaders = 1)
    val inputPorts = Seq.fill(nDownstreamInwards)(Module(new InputPort(config)))
    for (i <- 0 until nDownstreamInwards) {
      upstream(i) <> inputPorts(i).io.outward
      inputPorts(i).io.inward(0) <> downstream(i)
    }
    // FIXME: We should probably return the whole Seq here?
    inputPorts
  }
}

class SingleValueInputPortInwardConnectionFactory[T1 <: Data](genData: T1) extends InputPortInwardConnectionFactory(
  genData,
  new SingleToken(genData),
) {}

class OutputPortPassthroughBuilder {

}

case class TopInputPortConfig[T1 <: Data, T2 <: SwToken[T1]](
                             genData: T1,
                             genToken: T2
                             )

  class TopInputPortIO[T1 <: Data, T2 <: SwToken[T1]](c: TopInputPortConfig[T1, T2]) extends Bundle {
  val fire = Input(Bool())
  val in = Input(new SwSingleToken(c.genData))
  val out = new EventWriteMaster(c.genData, new SingleToken(c.genData))

  def driveDefaults() = {
    out.driveDefaults()
  }
}
class TopInputPort[T1 <: Data, T2 <: SwToken[T1]](c: TopInputPortConfig[T1, T2]) extends Module {
  require(c.genToken.isInstanceOf[SwSingleToken[T1]])

  val io = IO(new TopInputPortIO(c))
  io.driveDefaults()

  when(io.fire) {
    io.out.req.valid := true.B
    io.out.req.token.data := io.in.data
    io.out.req.present := io.in.present
    io.out.fire := true.B
  }

  assert(!(io.fire && RegNext(io.fire)), "[Port.scala] TopInputPort triggered in two consecutive cycles]")
}
case class TopOutputPortConfig[T1 <: Data, T2 <: SwToken[T1]](
                                             genData: T1,
                                             genToken: T2
                                           )

class TopOutputPortIO[T1 <: Data, T2 <: SwToken[T1]](c: TopOutputPortConfig[T1, T2]) extends Bundle {
  val fire = Output(Bool())
  val out = Output(c.genToken)
  val in = new EventReadMaster(c.genData, new SingleToken(c.genData))

  def driveDefaults() = {
    in.driveDefaults()
    fire := false.B
    out := 0.U.asTypeOf(c.genToken)
  }
}

// FIXME: Think of how we support the other ports
class TopOutputPort[T1 <: Data, T2 <: SwToken[T1]](c: TopOutputPortConfig[T1, T2]) extends Module {
  require(c.genToken.isInstanceOf[SwSingleToken[T1]])
  val io = IO(new TopOutputPortIO(c))
  io.driveDefaults()

  val data = RegInit(0.U.asTypeOf(c.genToken)).asInstanceOf[SwSingleToken[T1]]
  io.out := data

  when(io.in.resp.valid) {
    data.data := io.in.resp.token.data
    data.present := io.in.resp.present
    io.fire := true.B
    io.in.fire := true.B
  }
  assert(!(io.in.resp.valid && RegNext(io.in.resp.valid)), "[Port.scala] TopOutputPort fired twice in row")
}

class UnconnectedInputPortIO[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val writeAbsent = Input(Bool())
  val write = new EventWriteMaster(genData, genToken)
}

class UnconnectedInputPort[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Module {
  val io = IO(new UnconnectedInputPortIO(genData, genToken))
  io.write.driveDefaults()
  when(io.writeAbsent) {
    io.write.writeAbsentAndFire()
  }
}