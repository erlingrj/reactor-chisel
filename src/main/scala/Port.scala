package reactor

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._

import scala.collection.mutable.ArrayBuffer


abstract class ReactorPortIO[T1 <: Data, T2 <: Token[T1]] extends Bundle {

}
case class InputPortArbiterConfig[T1 <: Data, T2 <: Token[T1]] (
                                   genData: T1,
                                   genToken : T2,
                                   nReaders: Int
                                 ) {
  def nReadersWidth = if (nReaders == 1) 1.W else log2Ceil(nReaders).W
}

// Concrete implementation to avoid type erasure problems: https://stackoverflow.com/questions/40237011/non-variable-type-argument
trait InputPortArbiterSingleConfig[T1 <: Data] extends InputPortArbiterConfig[T1, SingleToken[T1]]
trait InputPortArbiterArrayConfig[T1 <: Data] extends InputPortArbiterConfig[T1, ArrayToken[T1]]
trait InputPortArbiterPureConfig extends InputPortArbiterConfig[UInt, PureToken]


abstract class InputPortArbiterIO[T1 <: Data, T2 <: Token[T1]](c: InputPortArbiterConfig[T1,T2]) extends ReactorPortIO[T1,T2] {
  val inward: Vec[TokenReadSlave[T1,T2]]
  val outward: TokenReadMaster[T1,T2]

  def driveDefaults(): Unit = {
    inward.foreach(_.driveDefaults())
    outward.driveDefaults()
  }

  def plugInwards(): Unit = {
    inward.foreach(i => {
      i.driveDefaultsFlipped()
    })
  }
}
class InputPortArbiterSingleValueIO[T1 <: Data](c: InputPortArbiterConfig[T1, SingleToken[T1]]) extends InputPortArbiterIO(c) {
  val inward = Vec(c.nReaders, new SingleTokenReadSlave(c.genData))
  val outward = new SingleTokenReadMaster(c.genData)
}
class InputPortArbiterPureIO(c: InputPortArbiterConfig[UInt, PureToken]) extends InputPortArbiterIO(c) {
  val inward = Vec(c.nReaders, new PureTokenReadSlave)
  val outward = new PureTokenReadMaster
}
class InputPortArbiterArrayIO[T1 <: Data](c: InputPortArbiterConfig[T1, ArrayToken[T1]]) extends InputPortArbiterIO(c) {
  val inward = Vec(c.nReaders, new ArrayTokenReadSlave(c.genData, c.genToken))
  val outward = new ArrayTokenReadMaster(c.genData, c.genToken)
}

abstract class InputPortArbiter[T1 <: Data, T2 <: Token[T1]](c: InputPortArbiterConfig[T1,T2]) extends Module {
  val io: InputPortArbiterIO[T1,T2]
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
      when(regCount === 0.U && io.outward.token) {
        regTokens.foreach(_ := true.B)
      }

      for (i <- 0 until c.nReaders) {
        io.inward(i).resp.valid := regTokens(i)
        io.inward(i).token := regTokens(i) && io.outward.token
        io.inward(i).present := regTokens(i) && io.outward.present

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

class InputPortArbiterSingleValue[T1 <: Data](c: InputPortArbiterConfig[T1, SingleToken[T1]]) extends InputPortArbiter(c) {
  val io = IO(new InputPortArbiterSingleValueIO(c))
  main()
  var downstreamIdx = 0
  var upstreamConnected = false

  def >>(down: SingleTokenReadMaster[T1]): Unit = {
    io.inward(downstreamIdx) <> down
    downstreamIdx += 1
  }
  def >>(downs: Seq[SingleTokenReadMaster[T1]]): Unit = downs.foreach(this >> _)

  def <<(up: SingleTokenReadMaster[T1]): Unit = {
    require(!upstreamConnected, "[Port.scala] connectUpstream called twice on InputPort")
    io.outward <> up
    upstreamConnected = true
  }
  def <<(up: Vec[SingleTokenReadMaster[T1]]): Unit = this << up(0)
}

class InputPortArbiterArray[T1 <: Data](c: InputPortArbiterConfig[T1, ArrayToken[T1]]) extends InputPortArbiter(c) {
  val io = IO(new InputPortArbiterArrayIO(c))
  main()
  var downstreamIdx = 0
  var upstreamConnected = false

  def >>(down: ArrayTokenReadMaster[T1]): Unit = {
    io.inward(downstreamIdx) <> down
    downstreamIdx += 1
  }

  def >>(downs: Seq[ArrayTokenReadMaster[T1]]): Unit = downs.foreach(this >> _)

  def <<(up: ArrayTokenReadMaster[T1]): Unit = {
    require(!upstreamConnected, "[Port.scala] connectUpstream called twice on InputPort")
    io.outward <> up
    upstreamConnected = true
  }

  def <<(up: Vec[ArrayTokenReadMaster[T1]]): Unit = this << up(0)
}
class InputPortArbiterPure[T1 <: Data](c: InputPortArbiterConfig[UInt, PureToken]) extends InputPortArbiter(c) {
  val io = IO(new InputPortArbiterPureIO(c))
  main()

  var downstreamIdx = 0
  var upstreamConnected = false

  def >>(down: PureTokenReadMaster): Unit = {
    io.inward(downstreamIdx) <> down
    downstreamIdx += 1
  }

  def >>(downs: Seq[PureTokenReadMaster]): Unit = downs.foreach(this >> _)

  def <<(up: PureTokenReadMaster): Unit = {
    require(!upstreamConnected, "[Port.scala] connectUpstream called twice on InputPort")
    io.outward <> up
    upstreamConnected = true
  }

  def <<(up: Vec[PureTokenReadMaster]): Unit = this << up(0)
}

case class OutputPortArbiterConfig[T1 <: Data, T2 <: Token[T1]](
                                      genData: T1,
                                      genToken: T2,
                                      nWriters: Int
                                      ) {
  def nWritersWidth: Width = if (nWriters == 1) 1.W else log2Ceil(nWriters).W
}
abstract class OutputPortArbiterIO[T1 <: Data, T2 <: Token[T1]](c: OutputPortArbiterConfig[T1, T2]) extends ReactorPortIO[T1,T2] {
  val inward: Vec[TokenWriteSlave[T1,T2]]
  val outward: TokenWriteMaster[T1,T2]

  def driveDefaults(): Unit = {
    inward.foreach(_.driveDefaults())
    outward.driveDefaults()
  }

  def plugInwards(): Unit = {
    inward.foreach(_.driveDefaultsFlipped())
  }
}

class OutputPortArbiterSingleValueIO[T1 <: Data](c: OutputPortArbiterConfig[T1, SingleToken[T1]]) extends OutputPortArbiterIO(c) {
  val inward = Vec(c.nWriters, new SingleTokenWriteSlave(c.genData))
  val outward = new SingleTokenWriteMaster(c.genData)
}
class OutputPortArbiterPureIO(c: OutputPortArbiterConfig[UInt, PureToken]) extends OutputPortArbiterIO(c) {
  val inward = Vec(c.nWriters, new PureTokenWriteSlave)
  val outward = new PureTokenWriteMaster
}
class OutputPortArbiterArrayIO[T1 <: Data](c: OutputPortArbiterConfig[T1, ArrayToken[T1]]) extends OutputPortArbiterIO(c) {
  val inward = Vec(c.nWriters, new ArrayTokenWriteSlave(c.genData, c.genToken))
  val outward = new ArrayTokenWriteMaster(c.genData, c.genToken)
}

abstract class OutputPortArbiter[T1 <: Data, T2 <: Token[T1]](c: OutputPortArbiterConfig[T1, T2]) extends Module {
  val io: OutputPortArbiterIO[T1,T2]

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

//    assert(!(io.outward.fire && !io.outward.req.ready))
  }

  var upstreamIdx = 0
  var downstreamConnected = false
}

class OutputPortArbiterSingleValue[T1 <: Data](c: OutputPortArbiterConfig[T1, SingleToken[T1]]) extends OutputPortArbiter(c){
  val io = IO(new OutputPortArbiterSingleValueIO(c))
  main()

  def <<(up: SingleTokenWriteMaster[T1]): Unit = {
    io.inward(upstreamIdx) <> up
    upstreamIdx += 1
  }
  def <<(ups: Seq[SingleTokenWriteMaster[T1]]): Unit = ups.foreach( this << _)

  def >>(down: SingleTokenWriteMaster[T1]): Unit = {
    require(!downstreamConnected)
    io.outward <> down
    downstreamConnected = true
  }
}
class OutputPortArbiterArray[T1 <: Data](c: OutputPortArbiterConfig[T1, ArrayToken[T1]]) extends OutputPortArbiter(c){
  val io = IO(new OutputPortArbiterArrayIO(c))
  main()

  def <<(up: ArrayTokenWriteMaster[T1]): Unit = {
    io.inward(upstreamIdx) <> up
    upstreamIdx += 1
  }

  def <<(ups: Seq[ArrayTokenWriteMaster[T1]]): Unit = ups.foreach(this << _)

  def >>(down: ArrayTokenWriteMaster[T1]): Unit = {
    require(!downstreamConnected)
    io.outward <> down
    downstreamConnected = true
  }
}

class OutputPortArbiterPure[T1 <: Data](c: OutputPortArbiterConfig[T1, ArrayToken[T1]]) extends OutputPortArbiter(c){
  val io = IO(new OutputPortArbiterArrayIO(c))
  main()

  def <<(up: PureTokenWriteMaster): Unit = {
    io.inward(upstreamIdx) <> up
    upstreamIdx += 1
  }

  def <<(ups: Seq[PureTokenWriteMaster]): Unit = ups.foreach(this << _)

  def >>(down: PureTokenWriteMaster): Unit = {
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
class InputPortArbiterInwardConnectionFactory[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends CircuitFactory {

  // Array of the downstream Input ports of contained reactors
  // An ArrayBuffer is used to "iteratively" grow this array
  var downstream: ArrayBuffer[TokenReadMaster[T1,T2]] = ArrayBuffer()

  // A Seq of Input ports in the parent reactor. Here we use an immutable Seq. Since we only create this
  // once, at construction.
  var upstream: Seq[TokenReadMaster[T1, T2]] = Seq()

  def nDownstreamInwards = downstream.length

  // Declare that a contained input port is downstream. Here the argument is a Seq of TokenReadMaster (e.g. input ports)
  // This is because the contained input port might have multiple "channels" because it itself might have contained reactors
  // to which it forwards the connection.
  def declareDownstream(down: Seq[TokenReadMaster[T1,T2]]) = {
    down.foreach(downstream += _)
  }
  // Convenient shorthand for declareDownstream
  def >>(down: Seq[TokenReadMaster[T1,T2]]) = {
    declareDownstream(down)
  }

  // Declare the Input port in the parent reactor. Check the width.
  // Either it is equal the number of downstreams, or it is 1 greater (due to an internal port)
  def declareInput(up: Seq[TokenReadMaster[T1, T2]]) = {
    if (up.length == nDownstreamInwards) upstream = up
    else if (up.length == (nDownstreamInwards + 1)) upstream = up.drop(1)
    else require(false)
  }
  def <<(up: Seq[TokenReadMaster[T1, T2]]) = {
    declareInput(up)
  }

  // This constructs the actual h
  def construct(): Seq[Module] = {
    val inputPorts = Seq.fill(nDownstreamInwards)(
      Module(
        genToken match {
          case t: SingleToken[T1] => new InputPortArbiterSingleValue(InputPortArbiterConfig(genData, new SingleToken(genData), 1))
          case t: ArrayToken[T1] => new InputPortArbiterArray(InputPortArbiterConfig(genData, genToken.asInstanceOf[ArrayToken[T1]],1))
          case t: PureToken => new InputPortArbiterPure(InputPortArbiterConfig(UInt(0.W), new PureToken, 1))
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

class SingleValueInputPortArbiterInwardConnectionFactory[T1 <: Data](genData: T1) extends InputPortArbiterInwardConnectionFactory(
  genData,
  new SingleToken(genData),
) {}

abstract class UnconnectedInputPortIO[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val writeAbsent = Input(Bool())
  val write: TokenWriteMaster[T1,T2]
}

class UnconnectedSingleValueInputPortIO[T1 <: Data](genData: T1) extends UnconnectedInputPortIO(genData, new SingleToken(genData)) {
  val write = new SingleTokenWriteMaster(genData)
}

class UnconnectedArrayInputPortIO[T1 <: Data](genData: T1, genToken: ArrayToken[T1]) extends UnconnectedInputPortIO(genData, genToken) {
  val write = new ArrayTokenWriteMaster(genData, genToken)
}

class UnconnectedPureInputPortIO extends UnconnectedInputPortIO(UInt(0.W),new PureToken) {
  val write = new PureTokenWriteMaster
}

abstract class UnconnectedInputPort[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Module {
  val io: UnconnectedInputPortIO[T1,T2]

  def main() = {
    io.write.driveDefaults()
    when(io.writeAbsent) {
      io.write.writeAbsent()
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
