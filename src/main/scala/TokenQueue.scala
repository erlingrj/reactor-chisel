package reactor

import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer

/**
 * First some utility scala functions for calculating the hyperperiod and schedule based on
 * a set of Timers.
 */
case class ScheduleElement(tag: Time, triggers: Seq[Boolean])
object Schedule {
  type EventSchedule = Seq[ScheduleElement]

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  /**
   * Calculate the hyperperiod of the timers. We must consider both periods and offsets.
   * @param configs
   * @return
   */
  def calculateHyperPeriod(configs: Seq[TimerTriggerConfig]): Time = {
    Time.nsec(
      configs
        .filterNot(_.period.ticks == 0)
        .map(_.period.ticks)
        .reduceOption(lcm).getOrElse(0)
    )
  }

  // Calculate the steps present in the schedule.
  def calculateScheduleSteps(hyperPeriod: Time, configs: Seq[TriggerConfig]): Seq[Time] = {
    val steps = ArrayBuffer[Time]()
    for (c <- configs) {
      c match {
        case c: TimerTriggerConfig =>
          var time = c.offset
          while (time == 0 || time < hyperPeriod) {
            if (!steps.contains(time)) steps += time
            if (c.period > 0) time += c.period else time = Time.FOREVER
          }
        case _: StartupTriggerConfig => if (!steps.contains(Time.nsec(0))) steps += Time.nsec(0)
        case _ =>
      }
    }
    steps.sorted.toSeq
  }

  def printSchedules(scheds: (EventSchedule, EventSchedule)): Unit = {
    println("**** Initial Schedule: ")
    for (el <- scheds._1.zipWithIndex) {
      println(s"${el._2}. Tag=${el._1.tag} TriggerVec=${el._1.triggers}")
    }

    println("**** Periodic Schedule: ")
    for (el <- scheds._2.zipWithIndex) {
      println(s"${el._2}. Tag=${el._1.tag} TriggerVec=${el._1.triggers}")
    }
  }

  def createSchedules(configs: Seq[TriggerConfig]): (Time, EventSchedule, EventSchedule, ScheduleElement)= {
    val timers = configs.collect{ case t: TimerTriggerConfig => t}

    val hyperPeriod = calculateHyperPeriod(timers)
    val periodicSchedule = ArrayBuffer[ScheduleElement]()
    val initialSchedule = ArrayBuffer[ScheduleElement]()
    val steps = calculateScheduleSteps(hyperPeriod, configs)
    println(s"Hyper period is: ${hyperPeriod.ticks} with steps: `$steps`")

    for (i <- steps) {
      // The periodic schedule is all timers with periods greater than zero
      val periodic = configs.map {
        case c: TimerTriggerConfig => c.period > 0 && (i.nsec - c.offset.nsec) % c.period.nsec == 0
        case _ => false
      }

      // Initial schedule is just all timers
      val initial = configs.map {
        case c: TimerTriggerConfig => c.offset.nsec == i.nsec
        case _: StartupTriggerConfig => i.nsec == 0
        case _ => false
      }


      periodicSchedule += ScheduleElement(i, periodic)
      initialSchedule += ScheduleElement(i, initial)
    }

    val shutdownTriggers = configs.map {
      case c: ShutdownTriggerConfig => true
      case _ => false
    }

    val shutdown = ScheduleElement(Time.FOREVER, shutdownTriggers)

    (hyperPeriod, initialSchedule.toSeq, periodicSchedule.toSeq, shutdown)
  }
}

/**
 * The parameters needed to build an Event Queue
 * @param nLocalTriggers
 * @param hyperPeriod
 * @param shutdownTime
 * @param shutdownTriggers
 * @param initialSchedule
 * @param periodicSchedule
 */
case class TokenQueueParams(
    nLocalTriggers: Int,
  nTimers: Int,
  nPhyActions: Int,
    hyperPeriod: Time,
    shutdownTime: Time,
    shutdownTriggers: ScheduleElement,
    initialSchedule: Schedule.EventSchedule,
    periodicSchedule: Schedule.EventSchedule
) {
  def scheduleLength = initialSchedule.size
  def scheduleLengthBits = if (scheduleLength > 0) Math.max(log2Ceil(scheduleLength), 1) else 0
  def scheduleWidth = nLocalTriggers
  def scheduleWidthBits = Math.max(log2Ceil(scheduleWidth), 1)

  require(nLocalTriggers == (nTimers + nPhyActions))
  require(nLocalTriggers == shutdownTriggers.triggers.size, "[TokenQueue] Mismatch between nLocalTriggers and the widths of the schedules")
}

class TokenQueueIO(p: TokenQueueParams) extends Bundle {
  val nextEventTag = Decoupled(Tag())
  val triggerVec = Vec(p.scheduleWidth, Output(Bool()))
  val terminate = Output(Bool())


  def driveDefaultsFlipped(): Unit = {
    nextEventTag.ready := false.B
  }
  def driveDefaults(): Unit  = {
    nextEventTag.bits := Tag.NEVER
    nextEventTag.valid := false.B
    terminate := false.B
    triggerVec.map(_ := false.B)
  }
}

/**
 * Has two schedules. One for the very first hyperperiod, and one for the rest.
 * This puts a limitation on that the offset cannot be greater than the period. This is too limiting and should be
 * fixed later. We also handle the shutdown.
 * initialSchedule and periodicSchedule must contain the VERY SAME steps. Which means that we can have some totally absent
 * steps in the periodicSchedule. This should be fixed. An edge case that complicates things is when we only have initial events
 * and no periodic. E.g. only startup and shutdown events.
 * @param p
 */
// FIXME: There are more efficient ways of handling the initial/periodic schedule. Also consider when they are identical
class TokenQueue(p: TokenQueueParams) extends Module {
  val io = IO(new TokenQueueIO(p))
  io.driveDefaults()

  val regNET = RegInit(Tag.NEVER)
  val regNETValid = RegInit(false.B)

  io.nextEventTag.bits := regNET
  io.nextEventTag.valid := regNETValid

  // Only bother with the event queue if we actually have locally originating events.
  if (p.scheduleLength > 0) {
    val initialRound = RegInit(true.B)

    val regIdx = RegInit(0.U(p.scheduleLengthBits.W))
    val triggerVecInital = RegInit(VecInit(Seq.tabulate(p.initialSchedule.length) { i =>
      VecInit(Seq.tabulate(p.initialSchedule(0).triggers.size) { j =>
        p.initialSchedule(i).triggers(j).B
      })
    }))

    val triggerVecPeriodic = RegInit(VecInit(Seq.tabulate(p.initialSchedule.length) { i =>
      VecInit(Seq.tabulate(p.periodicSchedule(0).triggers.size) { j =>
        p.periodicSchedule(i).triggers(j).B
      })
    }))

    // Do first the initial round. Then the periodic (if we have one)
    when(initialRound) {
      for ((triggerIO, trigger) <- io.triggerVec zip triggerVecInital(regIdx)) {
        triggerIO := trigger
      }
    }.otherwise {
      if (p.hyperPeriod > 0) {
        for ((triggerIO, trigger) <- io.triggerVec zip triggerVecPeriodic(regIdx)) {
          triggerIO := trigger
        }
      }
    }
    val tags = RegInit(VecInit(Seq.tabulate(p.initialSchedule.size) { i => Tag(p.initialSchedule(i).tag) }))
    val regEpoch = RegInit(Tag(0))

    val sIdle :: sStep1 :: sStep2 :: Nil = Enum(3)
    val regState = RegInit(sStep2) // Initialize to step2 were we compute the NET


    switch (regState) {
      is (sIdle) {
        when(io.nextEventTag.fire) {
          regState := sStep1
          regNETValid := false.B
        }
      }

      is (sStep1) {
        regIdx := regIdx + 1.U

        when(regIdx === (p.scheduleLength - 1).U) {
          // After first wrap, we start using the periodic schedule
          initialRound := false.B
          regIdx := 0.U
          regEpoch := regEpoch + p.hyperPeriod.ticks.U
        }
        regState := sStep2
      }

      is (sStep2) {
        regNETValid := true.B

        // In the special case that we dont have a hyperperiod. I.e. there arent any periodic triggers on the FPGA
        //  then we set the NEVER tag as the NET
        if (p.hyperPeriod == 0) {
         when (initialRound) {
           regNET := tags(regIdx) + regEpoch
         }.otherwise {
           regNET := Tag.NEVER
         }
        } else {
          regNET := tags(regIdx) + regEpoch
        }

        regState := sIdle
      }
    }

  } else {
    println("Empty schedule. Only compiling a shell for the TokenQueue")
    regNETValid := true.B
    regNET := Tag.NEVER
  }

  assert(!(io.nextEventTag.bits === Tag.NEVER && io.nextEventTag.fire))
  assert(!(io.nextEventTag.ready && !io.nextEventTag.valid))
}

class TokenQueueStandalone(p: TokenQueueParams) extends TokenQueue(p) {
  if (p.shutdownTime != Time.NEVER) {
    val regDone = RegInit(false.B)

    when(regNETValid && (regNET>= p.shutdownTime.ticks.U)) {
      io.nextEventTag.bits := p.shutdownTime.ticks.U
      when (regNETValid && regNET > p.shutdownTime.ticks.U) {
        // We have a shutdown trigger at an independent tag. Only trigger shutdown reactions
        for ((triggerIO, trigger) <- io.triggerVec zip p.shutdownTriggers.triggers) {
          triggerIO := trigger.B
        }
      }.otherwise {
        // We have a shutdown trigger simultanous with other triggers. Only enable shutdown reactions. Dont tluch other
        // triggers.
        for (idx <- p.shutdownTriggers.triggers.indices) {
          if (p.shutdownTriggers.triggers(idx)) {
            io.triggerVec(idx) := true.B
          }
        }
      }
      // Wait for acceptance of this event
      when(io.nextEventTag.fire) {
        regDone := true.B
      }
    }

    // Set next NET to NEVER and suggest termination
    when(regDone) {
      io.nextEventTag.bits := Tag.NEVER
      io.nextEventTag.valid := false.B
      io.terminate := true.B
    }
    assert(!(io.nextEventTag.fire && regDone))
  }
}


class TokenQueueCodesign(p: TokenQueueParams) extends TokenQueue(p) {
  val shutdownIO = IO(Input(new ShutdownCommand()))
  val tagIO = IO(Input(Tag()))

  def doShutdown = shutdownIO.valid
  val regDone = RegInit(false.B)

  when(doShutdown) {
     // FIXME: Does this really work or are we just terminating from SW?
    when(!shutdownIO.independent) {
      // Shutdown simultaneous with the local next event tag. Dont touch anything, just enable the shutdown triggers
      for (idx <- p.shutdownTriggers.triggers.indices) {
        if (p.shutdownTriggers.triggers(idx)) {
          io.triggerVec(idx) := true.B
        }
      }
    }.otherwise {
      // We do shutdown at an independent tag.
      io.nextEventTag.bits := tagIO
      for ((triggerIO, trigger) <- io.triggerVec zip p.shutdownTriggers.triggers) {
        triggerIO := trigger.B
      }
    }
    when(io.nextEventTag.fire) {
      regDone := true.B
    }
  }

  when(regDone) {
    io.terminate := true.B
    io.nextEventTag.valid := false.B
    io.nextEventTag.bits := Tag.FOREVER
  }
}

class PhysicalActionTokenQueueIO(nPhysicalActions: Int, nTimers: Int) extends Bundle {
  val phySchedules = Vec(nPhysicalActions, new PureTokenWriteSlave)
  val nextEventTag = Decoupled(Tag())
  val triggerVec = Output(Vec(nTimers+nPhysicalActions, Bool()))

  def driveDefaults()= {
    triggerVec.foreach(_ := false.B)
    phySchedules.foreach(_.driveDefaults())
    nextEventTag.valid := false.B
    nextEventTag.bits := 0.U
  }

  def driveDefaultsFlipped() = {
    nextEventTag.nodeq()
  }
}

class PhysicalActionTokenQueue(p: TokenQueueParams) extends Module {
  val io = IO(new PhysicalActionTokenQueueIO(p.nPhyActions, p.nTimers))
  io.driveDefaults()

  if(p.nPhyActions > 0) {
    val regNET = RegInit(0.U.asTypeOf(Tag()))
    val regValids = RegInit(VecInit(Seq.fill(p.nPhyActions)(false.B)))

    def isBusy = regValids.asUInt.orR

    io.nextEventTag.valid := isBusy
    io.nextEventTag.bits := regNET

    // TriggerVec is prepended with the timer triggers which are all absent. (0.U(ntimers.W))
    for (i <- 0 until p.nPhyActions) {
      io.triggerVec(i + p.nTimers) := regValids(i)
    }

    // Current limitation. Only one outstanding physical action
    for ((phy, idx) <- io.phySchedules.zipWithIndex) {
      phy.req.ready := !isBusy
      when(phy.fire) {
        regValids(idx) := true.B
        regNET := phy.tag
      }
    }
    when(io.nextEventTag.fire) {
      regValids.foreach(_ := false.B)
    }

  }
}
class PhysicalActionConnectorIO(mainIO: ReactorPhysicalIO, extIO: ReactorPhysicalFlippedIO) extends Bundle {
  val main = mainIO
  val ext = extIO
  val triggers = Vec(ext.getAllPorts.size, new PureTokenWriteMaster)
  val schedules = Vec(ext.getAllPorts.size, new PureTokenWriteSlave)
}


object PhysicalActionConnector {
  def apply(mainIO: ReactorPhysicalIO, extIO: ReactorPhysicalFlippedIO, triggerGenIO: TriggerGeneratorIO): Unit = {
    for (((main, ext), idx) <- (mainIO.getAllPorts zip extIO.getAllPorts).zipWithIndex) {
      val (connFactory, mainIO, extIO) = (main, ext) match {
        case (m: PureTokenReadMaster, e: PureTokenWriteSlave) =>
          val c = new PureConnectionFactory
          c << e
          c >> m
          (c, m, e)
        case (m: SingleTokenReadMaster[Data], e: SingleTokenWriteSlave[Data]) =>
          val c = new SingleValueConnectionFactory(e.genData)
          c << e
          c >> m
          (c, m, e)
        case (m: ArrayTokenReadMaster[Data], e: ArrayTokenWriteSlave[Data]) =>
          val c = new ArrayConnectionFactory(e.genData, e.genToken)
          c << e
          c >> m
          (c, m, e)
      }
      val conn = connFactory.construct()
      val trigGenTrigger = triggerGenIO.phyTriggers(idx)
      val trigGenSched = triggerGenIO.phySchedules(idx)
      // Let the TriggerGenerator control when this connection fires and the tag it will be associated with
      conn.head.io.write.fire := trigGenTrigger.fire
      conn.head.io.write.absent := trigGenTrigger.absent
      conn.head.io.write.tag := trigGenTrigger.tag // FIXME: Hacky way of overriding the tag signal so that the TriggerGenerator decides the tag of any Physical Action event

      trigGenTrigger.req.ready := conn.head.io.write.req.ready
      trigGenTrigger.dat.nodeq()

      // Connect the fire signal from the top-level IO port to the TriggerGenerator
      trigGenSched.driveDefaultsFlipped()
      trigGenSched.fire := extIO.fire
    }
  }
}

class TokenQueueMuxIO(p: TokenQueueParams) extends Bundle {
  val nextEventTag = Decoupled(Tag())
  val triggerVec = Vec(p.scheduleWidth, Output(Bool()))
  val terminate = Output(Bool())
  val tagAdvanceGrant = Input(Tag())
  val shutdownCmd = Input(new ShutdownCommand)
  val phySchedules = Vec(p.nPhyActions, new PureTokenWriteSlave)

  def driveDefaults() = {
    nextEventTag.noenq()
    triggerVec.foreach(_ := false.B)
    terminate := false.B
  }

  def driveDefaultsFlipped() = {
    nextEventTag.nodeq()
    tagAdvanceGrant := 0.U
    shutdownCmd := 0.U.asTypeOf(new ShutdownCommand)
    phySchedules.foreach(_.driveDefaultsFlipped())
  }
}

class TokenQueueMux(p: TokenQueueParams)(implicit val cfg: GlobalReactorConfig) extends Module {
  val io = IO(new TokenQueueMuxIO(p))
  io.driveDefaults()


  val localEventQ = {
    if (cfg.standalone) Module(new TokenQueueStandalone((p)))
    else {
      val eq = Module(new TokenQueueCodesign(p))
      eq.shutdownIO := io.shutdownCmd
      eq.tagIO := io.tagAdvanceGrant
      eq
    }
  }
  localEventQ.io.driveDefaultsFlipped()
  val physicalEventQ = if (p.nPhyActions > 0) Some(Module(new PhysicalActionTokenQueue(p))) else None

  io.terminate := localEventQ.io.terminate

  if (!physicalEventQ.isDefined) {
    io.triggerVec := localEventQ.io.triggerVec
    io.nextEventTag <> localEventQ.io.nextEventTag
  } else {
    val phyEventQ = physicalEventQ.get
    phyEventQ.io.driveDefaultsFlipped()
    phyEventQ.io.phySchedules zip io.phySchedules foreach {f => f._1 <> f._2}

    val sComputeNet1 :: sWaitForLocal :: sWaitForExe :: Nil = Enum(3)
    val regState = RegInit(sComputeNet1)
    val regEventQPick = RegInit(false.B)

    switch(regState) {
      is (sComputeNet1) {
        regEventQPick := !phyEventQ.io.nextEventTag.valid || localEventQ.io.nextEventTag.bits < phyEventQ.io.nextEventTag.bits
        regState := sWaitForExe
      }

      is (sWaitForLocal) {
        when (localEventQ.io.nextEventTag.valid) {
          regState := sComputeNet1
        }
      }

      is (sWaitForExe) {
        when(regEventQPick) {
          io.nextEventTag <> localEventQ.io.nextEventTag
          io.triggerVec := localEventQ.io.triggerVec
        }.otherwise {
          io.nextEventTag <> phyEventQ.io.nextEventTag
          io.triggerVec := phyEventQ.io.triggerVec
        }
      }
    }

    when(phyEventQ.io.nextEventTag.valid && RegNext(phyEventQ.io.nextEventTag.valid)) {
      regState := sComputeNet1
    }

    when(!localEventQ.io.nextEventTag.valid) {
      regState := sWaitForLocal
    }
  }
}