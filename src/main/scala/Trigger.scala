package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror.directionOf

import scala.collection.mutable.ArrayBuffer



/**
 * The configuration of a timer
 * @param offset
 * @param period
 */
case class TimerConfig(
                        offset: Time,
                        period: Time,
                        builtin: Boolean,
                      ) {
  def isStartupTrigger: Boolean = (builtin && offset == 0)
  def isShutdownTrigger: Boolean = (builtin && offset == Time.FOREVER)
  def isBuiltinTrigger: Boolean = builtin
}



class TriggerIO extends Bundle {
  val trigger = new EventWriteMaster(UInt(0.W), new PureToken())
}

/**
 * Mainly used for coordinating shutdown and is thus primarily relevant for simulations. Not actual implementations
 */
class TimerCoordinationIO extends Bundle {
  val idle = Output(Bool())
}
/**
 * Due to our Dataflow semantics, the Timer needs to know about the <period, offset> of all other timers in the program
 * also. This is so that it can generate absent tokens at the trigger instants of those timers. Unless it has its own
 * trigger scheduled for this instant.
 *
 * FIXME: We might require some sort of buffering inside the connection in order to support e.g. timers with 1 tick
 *  unalignment. That will require all timers to generate events on both those ticks. And an assertion here will fail
 *
 *  FIXME: We should probably find the hyper-period of all the timers and generate ONE schedule for it to run through
 * @param c
 * @param others
 */
class Timer(c: TimerConfig, others: Seq[TimerConfig] = Seq())(globalReactorConfig: GlobalReactorConfig) extends Module {
  val io = IO(new TriggerIO)
  val coordinationIo = IO(new TimerCoordinationIO)
  coordinationIo.idle := false.B

  val now = RegInit((-1.S(64.W)).asTypeOf(UInt(64.W))) // Starts at UINT64_MAX
  // Advance logical time
  now := now + 1.U

  io.trigger.driveDefaults()

  val allTimers = Seq(c) ++ others

  val otherTimers = others.filterNot(_ == c).filterNot(_.builtin) // Filter out equal timers and startup triggers
  val hasStartupTrigger = allTimers.filter(_.builtin).filter(_.offset == 0).nonEmpty
  val hasShutdownTrigger = allTimers.filter(_.builtin).filter(_.offset == Time.FOREVER).nonEmpty

  if (otherTimers.nonEmpty) {
    val otherTimerWidths = for (t <- otherTimers) yield log2Ceil(1 + Math.max(t.period.ticks, t.offset.ticks))

    // Handle other timers
    for (i <- otherTimers.indices) {
      val regCountdownOther= RegInit((otherTimers(i).offset.ticks + 1).U(otherTimerWidths(i).W))
      regCountdownOther := regCountdownOther - 1.U

      when(regCountdownOther === 0.U) {
        assert(io.trigger.ready, "[Timer.scala] Absent Timer fired but downstream connection was busy")
        io.trigger.writeAbsent()
        io.trigger.fire := true.B
        regCountdownOther := (otherTimers(i).period.ticks - 1).U
      }

      // If a timeout was specified then freeze everything after this
      if (globalReactorConfig.timeout != Time.NEVER) {
        // FIXME: It is quite inefficient to do this comparison
        when(now > globalReactorConfig.timeout.ticks.U && now < Time.FOREVER.ticks.U) {
          regCountdownOther := Tag.FOREVER.time
        }
      }
    }
  }

  // If this is a normal timer. Do count down etc
  if (!c.isBuiltinTrigger) {
    val myTimerWidth = Math.max(1, log2Ceil(1 + Math.max(c.offset.ticks, c.period.ticks)))
    val regCountdownMe = RegInit((c.offset.ticks + 1).U(myTimerWidth.W))
    // Handle this timer (we do it later because we can then overwrite absent tokens written by the other
    regCountdownMe := regCountdownMe - 1.U

    when(regCountdownMe === 0.U) {
      assert(io.trigger.ready, "[Timer.scala] Timer fired but downstream connection was busy")
      io.trigger.write(0.U)
      io.trigger.req.token.tag := now
      io.trigger.fire := true.B
      regCountdownMe := (c.period.ticks - 1).U
    }

    // If a timeout was specified then freeze everything after this
    if (globalReactorConfig.timeout != Time.NEVER) {
      when(now > globalReactorConfig.timeout.ticks.U && now < Time.FOREVER.ticks.U) {
        regCountdownMe := Tag.FOREVER.time
        coordinationIo.idle := true.B
      }
    }
  } else {
    coordinationIo.idle := true.B
  }

  // Do optional startup trigger.
  if (hasStartupTrigger) {
    when(now === (-1.S).asTypeOf(UInt(64.W))) {
      assert(io.trigger.ready, "[Timer.scala] Startup timer fired but downstream connection was busy")
      if (c.isStartupTrigger) {
        io.trigger.write(0.U)
      } else {
        io.trigger.writeAbsent()
      }
      io.trigger.req.token.tag := 0.U
      io.trigger.fire := true.B
    }
  }

  // Do optional shutdown trigger.
  if (hasShutdownTrigger && globalReactorConfig.timeout != Time.NEVER) {
    when(now === (globalReactorConfig.timeout.ticks + 1).U) {
      assert(io.trigger.ready, "[Timer.scala] Shutdown trigger fired but downstream connection was busy")
      if (c.isShutdownTrigger) {
        io.trigger.write(0.U)
      } else {
        io.trigger.writeAbsent()
      }
      io.trigger.req.token.tag := globalReactorConfig.timeout.ticks.U
      io.trigger.fire := true.B
    }
  }
}


// A virtual timer is a placeholder for an actual LF timer. In LF we can declare an arbitrary number of timers and have
// them trigger reactions. These timers are translated into TimerVirtual in reactor-chisel. There will only be a single
// actual hardware timer which contains the counters a token-generation logic for all the other timers.

abstract class TriggerVirtual extends CircuitFactory {
  var triggeredReactions: ArrayBuffer[EventReadMaster[UInt, PureToken]] = ArrayBuffer()
  var inputTimerPort: EventWriteSlave[UInt, PureToken] = null;

  // Declare that this virtual timer triggers reaction `r`
  def declareTriggeredReaction(r: EventReadMaster[UInt, PureToken]): Unit = {
    triggeredReactions += r
  }

  // Declare that this virtual timer is driven by the timer input port `in`. This port
  // is eventually connected to the top-level timer module
  def declareInputPort(in: EventWriteSlave[UInt, PureToken]): Unit = {
    inputTimerPort = in
  }

  // This functions can be called after triggered reactions and the input-port is declared.
  // It will create a pure connection between the input port and the triggered reactions
  override def construct(): Seq[InputPort[UInt, PureToken]] = {
    val conn = new PureConnectionFactory()
    val in = Module(new InputPort(InputPortConfig(0.U, new PureToken, triggeredReactions.length)))

    conn.addUpstream(inputTimerPort)
    conn.addDownstream(Seq(in.io.outward))
    conn.construct()
    for (r <- triggeredReactions) {
      in >> r
    }
    Seq(in)
  }

}

class TimerTriggerVirtual(val cfg: TimerConfig) extends TriggerVirtual {}

class StartupTriggerVirtual extends TimerTriggerVirtual(TimerConfig(period = Time.nsec(0), offset = Time.nsec(0), builtin = true)) {}

class ShutdownTriggerVirtual extends TimerTriggerVirtual(TimerConfig(period = Time.nsec(0), offset = Time.FOREVER, builtin = true)) {}