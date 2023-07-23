/**
 * This file implements the Lingua Franca state variables. The challenge with state variables is that they live
 * in the scope of the Reactors, but they are accessible by the Reactions. This means that each Reaction must
 * have a read/write interface to the State Variable
 *
 * The state variable infrastructure includes a lot of duplication of the Event infrastructure found in Event.scala.
 * We have ReadReqs and WriteReqs communicated between StateReadMaster, StateReadSlaves, StateWriteMasters and StateWriteSlaves.
 * We reuse the Token infrastructure for holding values, this is because we anticipate the same types of State variables
 * as we have ports. I.e. SingleValue, Array and FIFO. Also the same type of interface. Immediate and Two-phase.
 */
package reactor


import chisel3._
import chisel3.experimental.DataMirror.directionOf
import chisel3.util._

/**
 * A StateReadReq is an interface between a StateReadMaster and a StateReadSlave. Dependent, on whether it is
 * an ArrayToken, FifoToken or SingleToken it might contain an address field or not
 * @param genData
 * @param genToken
 * @tparam T1
 * @tparam T2
 */
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

/**
 * A StateReadResp is sent from StateReadSlave back to StateReadMaster upon receiving a StateReadReq. It contains
 * a token.
 * @param genData
 * @param genToken
 * @tparam T1
 * @tparam T2
 */
class StateReadResp[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val data = genData

  def driveDefaults(): Unit = {
    if (directionOf(data) == ActualDirection.Output) {
      data := 0.U.asTypeOf(genData)
    }
  }
}

/**
 * A StateWriteReq is sent from a StateWriteMaster to a StateWriteSlave, it contains the data to be written, and
 * potentially an address.
 * @param genData
 * @param genToken
 * @tparam T1
 * @tparam T2
 */
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
      write := false.B
      data := 0.U.asTypeOf(genData)
    }
  }
}

/**
 * A StateReadMaster is an interface containing a `req` and `resp` field. A Reaction will contain StateReadMasters
 * for each StateVariable in the Reactor. The StateVariable itself will contain a StateReadSlave interace.
 * @param genData
 * @param genToken
 * @tparam T1
 * @tparam T2
 */
class StateReadMaster[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val req = Output(new StateReadReq(genData, genToken))
  val resp = Input(new StateReadResp(genData, genToken))

  def driveDefaults() = {
    req.driveDefaults()
    resp.driveDefaults()
  }
  def read(): T1 = {
    resp.data
  }
}

/**
 * A StateWriteMaster is an interface used by Reactions to write to state variables
 * @param genData
 * @param genToken
 * @tparam T1
 * @tparam T2
 */
class StateWriteMaster[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val req = Output(new StateWriteReq(genData, genToken))

  def driveDefaults() = {
    req.driveDefaults()
  }

  def write(d: T1): Unit = {
    req.write:= true.B
    genToken match {
      case _: SingleToken[T1] =>
        req.data := d
      case _: PureToken => // OK
      case _ => assert(false.B)
    }
  }
}

/**
 * A StateReadSlave is implemented inside the StateVariable to serve StateReadReqs.
 * @param genData
 * @param genToken
 * @tparam T1
 * @tparam T2
 */
class StateReadSlave[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val req = Input(new StateReadReq(genData, genToken))
  val resp = Output(new StateReadResp(genData, genToken))

  def driveDefaults() = {
    req.driveDefaults()
    resp.driveDefaults()
  }
}

/**
 * A StateWriteSlave is implemented inside the StateVariables to serve StateWriteReqs.
 * @param genData
 * @param genToken
 * @tparam T1
 * @tparam T2
 */
class StateWriteSlave[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val req = Input(new StateWriteReq(genData, genToken))

  def driveDefaults() = {
    req.driveDefaults()
  }
}

/**
 * A StateReadWriteMaster is a convenient way of grouping a ReadMaster and a WriteMaster in a single Bundle.
 * Each Reaction will have a StateReadWriteMaster interface per State variable in its parent Reactor.
 * @param genData
 * @param genToken
 * @tparam T1
 * @tparam T2
 */
class StateReadWriteMaster[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val read = new StateReadMaster(genData, genToken)
  val write = new StateWriteMaster(genData, genToken)

  def driveDefaults() = {
    read.driveDefaults()
    write.driveDefaults()
  }
}

/**
 * A StateReadWriteSlave is a convenient bundling of a StateReadSlave and a StateWriteSlave. A State Variable
 * will have one StateReadWriteSlave interface per reaction in the Reactor.
 * @param genData
 * @param genToken
 * @tparam T1
 * @tparam T2
 */
class StateReadWriteSlave[T1 <: Data, T2 <: Token[T1]](genData: T1, genToken: T2) extends Bundle {
  val read = new StateReadSlave(genData, genToken)
  val write = new StateWriteSlave(genData, genToken)
}

sealed trait StateProtocolOption
case object Immediate extends StateProtocolOption
case object SingleCycle extends StateProtocolOption
case object MultiCycle extends StateProtocolOption
/**
 * A StateConfig contains configuration of a StateVariable. It contains info about what type of Token and number of reactions
 * @param genData
 * @param genToken
 * @param nReactions
 * @tparam T1
 * @tparam T2
 */
case class StateConfig[T1 <: Data, T2 <: Token[T1]]
(
  genData: T1,
  genToken: T2,
  nReactions: Int,
  protocol: StateProtocolOption
)

/**
 * StateIO is the top-level IO of a State module.
 * @param c
 * @tparam T1
 * @tparam T2
 */
class StateIO[T1 <: Data, T2 <: Token[T1]](c: StateConfig[T1,T2]) extends Bundle {
  val ports = Vec(c.nReactions, new StateReadWriteSlave(c.genData, c.genToken))
}

/**
 * The abstract State class implements the State Variable functionality that is common to the different types
 * of state variables (SingleValue, Array and FIFO). The State variable exposes duplicate interfaces for each
 * reaction in the reactor. Due to mutual exclusion we do not have to arbitrate between the different interfaces.
 * We should make sure that it doesnt add any unnecessary overhead.
 * @param c
 * @tparam T1
 * @tparam T2
 */
abstract class State[T1 <: Data, T2 <: Token[T1]](c: StateConfig[T1, T2]) extends Module {
  val io = IO(new StateIO(c))

  var nReactions = 0
  def <> (reaction: StateReadWriteMaster[T1,T2]) = {
    require(c.nReactions > nReactions, s"[State.scala] Tried connecting more reactions to State, but only ${c.nReactions} reactions specified in the StateConfig")
    io.ports(nReactions) <> reaction
    nReactions += 1
  }
}


class SingleValueState[T1 <: Data](c: StateConfig[T1, SingleToken[T1]]) extends State(c) {

  assert(c.protocol == Immediate, "[State.scala] SingleValueState can only be used with the Immediate protocol")
  val data = RegInit(0.U.asTypeOf(c.genData))

  for (port <- io.ports) {
    port.read.resp.data := data
    when (port.write.req.write) {
      data := port.write.req.data
    }
  }

  assert(!(PopCount(io.ports.map(_.write.req.write)) > 1.U), "[State.scala] Multiple reactions tried writing to State at the same time")
}
