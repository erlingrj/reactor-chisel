package reactor

import chisel3.{UInt, _}
import chisel3.util._
import chisel3.experimental.DataMirror.directionOf

import scala.collection.mutable.ArrayBuffer


sealed trait TriggerConfig
case class TimerTriggerConfig(offset: Time, period: Time) extends TriggerConfig()
case class ShutdownTriggerConfig() extends TriggerConfig()
case class StartupTriggerConfig() extends TriggerConfig()
case class PhysicalActionTriggerConfig(offset: Time) extends TriggerConfig()



/**
 * Mainly used for coordinating shutdown and is thus primarily relevant for simulations. Not actual implementations
 */
class TimerCoordinationIO extends Bundle {
  val idle = Output(Bool())
}

// A virtual timer is a placeholder for an actual LF timer. In LF we can declare an arbitrary number of timers and have
// them trigger reactions. These timers are translated into TimerVirtual in reactor-chisel. There will only be a single
// actual hardware timer which contains the counters a token-generation logic for all the other timers.

class TriggerPureVirtual(val cfg: TriggerConfig) {

  var triggeredReactions: ArrayBuffer[EventPureReadMaster] = ArrayBuffer()
  var inputTimerPort: EventPureWriteSlave = null;

  // Declare that this virtual timer triggers reaction `r`
  def declareTriggeredReaction(r: EventPureReadMaster): Unit = {
    triggeredReactions += r
  }

  // Declare that this virtual timer is driven by the timer input port `in`. This port
  // is eventually connected to the top-level timer module
  def declareInputPort(in: EventPureWriteSlave): Unit = {
    inputTimerPort = in
  }

  // This functions can be called after triggered reactions and the input-port is declared.
  // It will create a pure connection between the input port and the triggered reactions
  def construct(): Seq[InputPort[UInt, PureToken]] = {
    val conn = new PureConnectionFactory()
    val in = Module(new InputPortPure(InputPortConfig(0.U, new PureToken, triggeredReactions.length)))

    conn.addUpstream(inputTimerPort)
    conn.addDownstream(Seq(in.io.outward))
    conn.construct()
    for (r <- triggeredReactions) {
      in >> r
    }
    Seq(in)
  }
}

class TimerTriggerVirtual(cfg: TimerTriggerConfig) extends TriggerPureVirtual(cfg) {}

class StartupTriggerVirtual extends TriggerPureVirtual(StartupTriggerConfig()) {}

class ShutdownTriggerVirtual extends TriggerPureVirtual(ShutdownTriggerConfig()) {}

class PhysicalActionTriggerPureVirtual(offset: Time) extends TriggerPureVirtual(PhysicalActionTriggerConfig(offset)) {}
