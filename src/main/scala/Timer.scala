package reactor

import chisel3._
import chisel3.experimental.DataMirror.directionOf
import chisel3.util._

import scala.collection.mutable.ArrayBuffer



/**
 * The configuration of a timer
 * @param offset
 * @param period
 */
case class TimerConfig(
                      offset: Time,
                      period: Time
                      ) {}

class MainTimerIO(nTimers: Int) extends Bundle {
  val timers = Vec(nTimers, new TimerIO())
  val nextEventTag = Output(new Tag)
  val tagAdvanceGrant = Input(new Tag)

  def driveDefaults(): Unit = {
    if (directionOf(nextEventTag) == ActualDirection.Output) {
      nextEventTag.time := 0.U
    } else {
      tagAdvanceGrant := 0.U
    }
  }
}

// FIXME: This MainTimer doesnt do any time coordination yet. It should have
class MainTimer(mainReactor: Reactor) extends Module {
  val mainReactorTimers = mainReactor.localTimers ++ mainReactor.containedTimers
  val mainReactorTimersIO = mainReactor.timerIO.localTimers ++ mainReactor.timerIO.containedTimers
  val io = IO(new MainTimerIO(mainReactorTimers.size))
  io.driveDefaults()

  println(s"MainTimer created for program where main has  ${mainReactor.containedTimers.size} contained + ${mainReactor.containedTimers.size} top-level timers")

  // Create the HW counters
  // FIXME: This should be optimized. I am reusing the old timer module, we can instead create a
  //  single timer with a single hyperperiod
  val allTimerConfigs = mainReactorTimers.map(_.cfg)
  val timers = for (t <- mainReactorTimers) yield Module(new Timer(t.cfg, allTimerConfigs.toSeq.diff(Seq(t.cfg))))
  // Connect the HW counters to top-level IO
  for ((timer, timerIO) <- timers zip io.timers) {
    timer.io.trigger <> timerIO.trigger
  }

}


class TimerIO extends Bundle {
  val trigger = new EventWriteMaster(UInt(0.W), new PureToken())
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
class Timer(c: TimerConfig, others: Seq[TimerConfig] = Seq()) extends Module {
  val io = IO(new TimerIO)
  val now = RegInit(0.U(64.W))
  io.trigger.driveDefaults()

  val othersOpt = others.filterNot(_ == c) // Filter out equal timers
  if (othersOpt.length > 0) {
    val otherTimerWidths = for (t <- others) yield log2Ceil(1 + Math.max(t.period.ticks, t.offset.ticks))

    val regCountdownOthers = RegInit(VecInit(Seq.tabulate(othersOpt.length)(i => othersOpt(i).offset.ticks.U(otherTimerWidths(i).W))))

    // Handle other timers
    for (i <- 0 until othersOpt.length) {
      regCountdownOthers(i) := regCountdownOthers(i) - 1.U

      when(regCountdownOthers(i) === 0.U) {
        assert(io.trigger.ready, "[Timer.scala] Absent Timer fired but downstream connection was busy")
        io.trigger.writeAbsent()
        io.trigger.fire := true.B
        regCountdownOthers(i) := othersOpt(i).period.ticks.U
      }
    }
  }

  val myTimerWidth = log2Ceil(1 + Math.max(c.offset.ticks, c.period.ticks))
  val regCountdownMe = RegInit((c.offset.ticks).U(myTimerWidth.W))
  // Handle this timer (we do it later because we can then overwrite absent tokens written by the other
  regCountdownMe := regCountdownMe - 1.U
  now := now + 1.U

  when (regCountdownMe === 0.U) {
    assert(io.trigger.ready, "[Timer.scala] Timer fired but downstream connection was busy")
    io.trigger.write(0.U)
    io.trigger.req.token.tag := now
    io.trigger.fire := true.B
    regCountdownMe := (c.period.ticks-1).U
  }
}


// A virtual timer is a placeholder for an actual LF timer. In LF we can declare an arbitrary number of timers and have
// them trigger reactions. These timers are translated into TimerVirtual in reactor-chisel. There will only be a single
// actual hardware timer which contains the counters a token-generation logic for all the other timers.
class TimerVirtual(val cfg: TimerConfig) extends CircuitFactory {

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

