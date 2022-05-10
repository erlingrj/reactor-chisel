package reactor

import chisel3._
import chisel3.util._




abstract class AbstractClockKeeper extends Module {

}

class NextEvent(implicit rc: ReactorGlobalParams) extends Bundle {
  val tag = TimeTag()
  val empty = Bool()
}

class TopClockKeeper(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends AbstractClockKeeper {

}

class ClockKeeperIO(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Bundle {

  val actionTags = Vec(c.actions.length, Flipped(Valid(TimeTag())))
  val actionsBusy = Vec(c.actions.length, Input(Bool()))

  val timerTags = Vec(c.timers.length, Flipped(Valid(TimeTag())))
  val timersBusy = Vec(c.timers.length, Input(Bool()))

  val reactorTags = Vec(c.reactors.length, Flipped(Valid(TimeTag())))
  val reactorsBusy = Vec(c.reactors.length, Input(Bool()))

  val logicalTimeOut = Decoupled(TimeTag())
  val logicalTimeIn = Flipped(Decoupled(TimeTag()))

  val nextEvent = Decoupled(TimeTag())
  val waitForFinish = Flipped(Decoupled())


  def tieOff(): Unit = {
    logicalTimeOut.valid := false.B
    logicalTimeOut.bits := TimeTag(0.U)
    logicalTimeIn.ready := false.B
    waitForFinish.ready := false.B
    nextEvent.bits := TimeTag(0.U)
    nextEvent.valid := false.B
  }

  def subElementsBusy(): Bool = {
    require(nSubElements() > 0) // FIXME: Also support cases when Reactor has only Reactions
    getBusy().reduce(_ || _)
  }

  def nSubElements(): Int = {
    c.reactors.length + c.actions.length + c.timers.length
  }
  def getTags(): IndexedSeq[Valid[TimeTag]] = {
    actionTags ++ timerTags ++ reactorTags
  }
  def getBusy(): IndexedSeq[Bool] = {
    actionsBusy ++ timersBusy++ reactorsBusy
  }
}


/**
 *
 * ClockKeeper is in the Contained Reactors
 * It listens for the nextEvents with Actions, Timers and containedReactors. Sorts them and forwards to super ClockKeeeper
 * It also accepts logicalTime from superClockKeeper and passes it on the subcomponents.
 * When the time is advanced it moves into execute stage until all sub-components are non-busy
 * It expects a "waitForFinish" input which goes high when all reactions have finished executing
 */
class ClockKeeper(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends AbstractClockKeeper {

  val io = IO(new ClockKeeperIO(c))
  io.tieOff()

  val regLogicalTime = RegInit(TimeTag(0.U))
  val regPhysicalTime = RegInit(TimeTag(0.U))

  val execSchedule = if(c.getNumReactorLevels() > 0) Some(Module(new ExecutionSchedule(c))) else None
  val sorter = Module(new TreeSorter(TimeTag(), io.nSubElements())).io
  sorter.tieOffExt


  val regNextEvent = RegInit(TimeTag(0.U))
  val regNextEventValid = RegInit(false.B)

  def connectSorter(): Unit = {
    for ((e,i) <- io.getTags().zipWithIndex) {
      sorter.in.bits(i) := Mux(e.valid, e.bits, TimeTag.max())
      assert(!(e.valid && e.bits <= regLogicalTime), "[ClockKeeper.scala] Got tag which is <= current logical time")
    }
    sorter.in.valid := io.getTags().map(_.valid).reduce(_||_)
    assert(!(sorter.in.valid && !sorter.in.ready), "[ClockKeeper.scala] Connnected Sorter to Tags but Sorter was not ready")
  }

  regPhysicalTime := regPhysicalTime + TimeTag(1.U)
  io.logicalTimeOut.bits := regLogicalTime

  io.nextEvent.bits := regNextEvent
  io.nextEvent.valid := regNextEventValid

  val sIdle :: sExec :: sBusy :: sSorting :: Nil = Enum(4)
  val regState = RegInit(sIdle)

  switch (regState) {
    is (sIdle) {
      io.logicalTimeIn.ready := true.B
      when (io.logicalTimeIn.fire) {
        regLogicalTime := io.logicalTimeIn.bits
        regState := sExec

        // If we get a new logical time >= our nextEvent then we invalidate it
        when (io.logicalTimeIn.bits >= regNextEvent && regNextEventValid) {
          regNextEventValid := false.B
        }

        assert(!((io.logicalTimeIn.bits.tag > regNextEvent.tag) && regNextEventValid), "[ClockKeeper.scala] ClockKeeper received logical time which was greater than its next event")
      }
      assert(!io.subElementsBusy(), "[ClockKeeper.scala] ClockKeeper in Idle mode but subelements busy")
    }

    is (sExec) {
      io.logicalTimeOut.bits := regLogicalTime
      io.logicalTimeOut.valid := true.B
      when (io.logicalTimeOut.fire) {
        regState := sBusy
      }
      assert(io.logicalTimeOut.fire, "[ClockKeeper.scala] Enter sExec but sub-components not ready")

    }
    is (sBusy) {
      io.waitForFinish.ready := !io.subElementsBusy()
      when(io.waitForFinish.fire) {
        // Go to sorting stage
        regState := sSorting
        connectSorter()
        when (sorter.in.fire) {
          regState := sSorting
        }. otherwise {
          regState := sIdle
          regNextEvent := TimeTag(0.U)
          regNextEventValid := false.B
        }
      }
    }

    is (sSorting) {
      sorter.out.ready := true.B
      when (sorter.out.fire) {
        regNextEvent := sorter.out.bits
        regNextEventValid := true.B
        regState := sIdle
      }
    }
  }
}




