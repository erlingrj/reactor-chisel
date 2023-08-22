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

// Concrete implementation to avoid type erasure problems: https://stackoverflow.com/questions/40237011/non-variable-type-argument
trait InputPortSingleValueConfig[T1 <: Data] extends InputPortConfig[T1, SingleToken[T1]]
trait InputPortArrayConfig[T1 <: Data] extends InputPortConfig[T1, ArrayToken[T1]]
trait InputPortPureConfig extends InputPortConfig[UInt, PureToken]


abstract class InputPortIO[T1 <: Data, T2 <: Token[T1]](c: InputPortConfig[T1,T2]) extends ReactorPortIO[T1,T2] {
  val inward: Vec[EventReadSlave[T1,T2]]
  val outward: EventReadMaster[T1,T2]

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
class InputPortSingleValueIO[T1 <: Data](c: InputPortConfig[T1, SingleToken[T1]]) extends InputPortIO(c) {
  val inward = Vec(c.nReaders, new EventSingleValueReadSlave(c.genData))
  val outward = new EventSingleValueReadMaster(c.genData)
}
class InputPortPureIO(c: InputPortConfig[UInt, PureToken]) extends InputPortIO(c) {
  val inward = Vec(c.nReaders, new EventPureReadSlave)
  val outward = new EventPureReadMaster
}
class InputPortArrayIO[T1 <: Data](c: InputPortConfig[T1, ArrayToken[T1]]) extends InputPortIO(c) {
  val inward = Vec(c.nReaders, new EventArrayReadSlave(c.genData, c.genToken))
  val outward = new EventArrayReadMaster(c.genData, c.genToken)
}

abstract class InputPort[T1 <: Data, T2 <: Token[T1]](c: InputPortConfig[T1,T2]) extends Module {
  val io: InputPortIO[T1,T2]
  def main() = {
    io.driveDefaults()
    io.inward.foreach(_ <> io.outward)

    // If we only have a single reader, then we just pass through the signal.
    // If there are more readers we do a state machine to control when the eat
    // signal is sent.
    if (c.nReaders > 1) {
      io.outward.fire := false.B

      val regCount = RegInit(0.U(c.nReadersWidth + 1.W))
      val regTokens = RegInit(VecInit(Seq.fill(c.nReaders)(false.B)))


      // New token from upstream
      when(regCount === 0.U && io.outward.resp.valid) {
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
        io.outward.fire := true.B
        regCount := 0.U
      }
      assert(util.PopCount(io.inward.map(_.fire)) <= 1.U, "[Port.scala] Multiple inwards connected reactions fired simultanously")

    }
  }
}

class InputPortSingleValue[T1 <: Data](c: InputPortConfig[T1, SingleToken[T1]]) extends InputPort(c) {
  val io = IO(new InputPortSingleValueIO(c))
  main()
  var downstreamIdx = 0
  var upstreamConnected = false

  def >>(down: EventSingleValueReadMaster[T1]): Unit = {
    io.inward(downstreamIdx) <> down
    downstreamIdx += 1
  }
  def >>(downs: Seq[EventSingleValueReadMaster[T1]]): Unit = downs.foreach(this >> _)

  def <<(up: EventSingleValueReadMaster[T1]): Unit = {
    require(!upstreamConnected, "[Port.scala] connectUpstream called twice on InputPort")
    io.outward <> up
    upstreamConnected = true
  }
  def <<(up: Vec[EventSingleValueReadMaster[T1]]): Unit = this << up(0)
}

class InputPortArray[T1 <: Data](c: InputPortConfig[T1, ArrayToken[T1]]) extends InputPort(c) {
  val io = IO(new InputPortArrayIO(c))
  main()
  var downstreamIdx = 0
  var upstreamConnected = false

  def >>(down: EventArrayReadMaster[T1]): Unit = {
    io.inward(downstreamIdx) <> down
    downstreamIdx += 1
  }

  def >>(downs: Seq[EventArrayReadMaster[T1]]): Unit = downs.foreach(this >> _)

  def <<(up: EventArrayReadMaster[T1]): Unit = {
    require(!upstreamConnected, "[Port.scala] connectUpstream called twice on InputPort")
    io.outward <> up
    upstreamConnected = true
  }

  def <<(up: Vec[EventArrayReadMaster[T1]]): Unit = this << up(0)
}
class InputPortPure[T1 <: Data](c: InputPortConfig[UInt, PureToken]) extends InputPort(c) {
  val io = IO(new InputPortPureIO(c))
  main()

  var downstreamIdx = 0
  var upstreamConnected = false

  def >>(down: EventPureReadMaster): Unit = {
    io.inward(downstreamIdx) <> down
    downstreamIdx += 1
  }

  def >>(downs: Seq[EventPureReadMaster]): Unit = downs.foreach(this >> _)

  def <<(up: EventPureReadMaster): Unit = {
    require(!upstreamConnected, "[Port.scala] connectUpstream called twice on InputPort")
    io.outward <> up
    upstreamConnected = true
  }

  def <<(up: Vec[EventPureReadMaster]): Unit = this << up(0)
}

case class OutputPortConfig[T1 <: Data, T2 <: Token[T1]](
                                      genData: T1,
                                      genToken: T2,
                                      nWriters: Int
                                      ) {
  def nWritersWidth: Width = if (nWriters == 1) 1.W else log2Ceil(nWriters).W
}
abstract class OutputPortIO[T1 <: Data, T2 <: Token[T1]](c: OutputPortConfig[T1, T2]) extends ReactorPortIO[T1,T2] {
  val inward: Vec[EventWriteSlave[T1,T2]]
  val outward: EventWriteMaster[T1,T2]

  def driveDefaults(): Unit = {
    inward.foreach(_.driveDefaults())
    outward.driveDefaults()
  }

  def plugInwards(): Unit = {
    inward.foreach(i => {
      i.fire := false.B
      i.req.valid := false.B
      i.req.token := DontCare
      i.req.present := false.B
    })
  }
}

class OutputPortSingleValueIO[T1 <: Data](c: OutputPortConfig[T1, SingleToken[T1]]) extends OutputPortIO(c) {
  val inward = Vec(c.nWriters, new EventSingleValueWriteSlave(c.genData))
  val outward = new EventSingleValueWriteMaster(c.genData)
}
class OutputPortPureIO(c: OutputPortConfig[UInt, PureToken]) extends OutputPortIO(c) {
  val inward = Vec(c.nWriters, new EventPureWriteSlave)
  val outward = new EventPureWriteMaster
}
class OutputPortArrayIO[T1 <: Data](c: OutputPortConfig[T1, ArrayToken[T1]]) extends OutputPortIO(c) {
  val inward = Vec(c.nWriters, new EventArrayWriteSlave(c.genData, c.genToken))
  val outward = new EventArrayWriteMaster(c.genData, c.genToken)
}

abstract class OutputPort[T1 <: Data, T2 <: Token[T1]](c: OutputPortConfig[T1, T2]) extends Module {
  val io: OutputPortIO[T1,T2]

  def main(): Unit = {
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

    assert(!(io.outward.fire && !io.outward.ready))
    assert(!(io.outward.req.valid && !io.outward.ready))
  }

  var upstreamIdx = 0
  var downstreamConnected = false
}

class OutputPortSingleValue[T1 <: Data](c: OutputPortConfig[T1, SingleToken[T1]]) extends OutputPort(c){
  val io = IO(new OutputPortSingleValueIO(c))
  main()

  def <<(up: EventSingleValueWriteMaster[T1]): Unit = {
    io.inward(upstreamIdx) <> up
    upstreamIdx += 1
  }
  def <<(ups: Seq[EventSingleValueWriteMaster[T1]]): Unit = ups.foreach( this << _)

  def >>(down: EventSingleValueWriteMaster[T1]): Unit = {
    require(!downstreamConnected)
    io.outward <> down
    downstreamConnected = true
  }
}
class OutputPortArray[T1 <: Data](c: OutputPortConfig[T1, ArrayToken[T1]]) extends OutputPort(c){
  val io = IO(new OutputPortArrayIO(c))
  main()

  def <<(up: EventArrayWriteMaster[T1]): Unit = {
    io.inward(upstreamIdx) <> up
    upstreamIdx += 1
  }

  def <<(ups: Seq[EventArrayWriteMaster[T1]]): Unit = ups.foreach(this << _)

  def >>(down: EventArrayWriteMaster[T1]): Unit = {
    require(!downstreamConnected)
    io.outward <> down
    downstreamConnected = true
  }
}

class OutputPortPure[T1 <: Data](c: OutputPortConfig[T1, ArrayToken[T1]]) extends OutputPort(c){
  val io = IO(new OutputPortArrayIO(c))
  main()

  def <<(up: EventPureWriteMaster): Unit = {
    io.inward(upstreamIdx) <> up
    upstreamIdx += 1
  }

  def <<(ups: Seq[EventPureWriteMaster]): Unit = ups.foreach(this << _)

  def >>(down: EventPureWriteMaster): Unit = {
    require(!downstreamConnected)
    io.outward <> down
    downstreamConnected = true
  }
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
    val inputPorts = Seq.fill(nDownstreamInwards)(
      Module(
        genToken match {
          case t: SingleToken[T1] => new InputPortSingleValue(InputPortConfig(genData, new SingleToken(genData), 1))
          case t: ArrayToken[T1] => new InputPortArray(InputPortConfig(genData, genToken.asInstanceOf[ArrayToken[T1]],1))
          case t: PureToken => new InputPortPure(InputPortConfig(UInt(0.W), new PureToken, 1))
        }
    ))
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

abstract class UnconnectedInputPortIO[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val writeAbsent = Input(Bool())
  val write: EventWriteMaster[T1,T2]
}

class UnconnectedSingleValueInputPortIO[T1 <: Data](genData: T1) extends UnconnectedInputPortIO(genData, new SingleToken(genData)) {
  val write = new EventSingleValueWriteMaster(genData)
}

class UnconnectedArrayInputPortIO[T1 <: Data](genData: T1, genToken: ArrayToken[T1]) extends UnconnectedInputPortIO(genData, genToken) {
  val write = new EventArrayWriteMaster(genData, genToken)
}

class UnconnectedPureInputPortIO extends UnconnectedInputPortIO(UInt(0.W),new PureToken) {
  val write = new EventPureWriteMaster
}

abstract class UnconnectedInputPort[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Module {
  val io: UnconnectedInputPortIO[T1,T2]

  def main() = {
    io.write.driveDefaults()
    when(io.writeAbsent) {
      io.write.writeAbsentAndFire()
    }
  }
}

class UnconnectedSingleValueInputPort[T1 <: Data](genData: T1) extends UnconnectedInputPort(genData, new SingleToken(genData)) {
  val io = IO(new UnconnectedSingleValueInputPortIO(genData))
  main()
}
class UnconnectedArrayInputPort[T1 <: Data](genData: T1, genToken: ArrayToken[T1]) extends UnconnectedInputPort(genData, genToken) {
  val io = IO(new UnconnectedArrayInputPortIO(genData, genToken))
  main()
}
class UnconnectedPureInputPort extends UnconnectedInputPort(UInt(0.W), new PureToken) {
  val io = IO(new UnconnectedPureInputPortIO)
  main()
}
