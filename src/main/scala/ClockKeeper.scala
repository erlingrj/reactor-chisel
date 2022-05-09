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

  val actionTags = Vec(c.actions.length, Decoupled(TimeTag()))
  val actionsBusy = Vec(c.actions.length, Input(Bool()))

  val timerTags = Vec(c.timers.length, Decoupled(TimeTag()))
  val timersBusy = Vec(c.timers.length, Input(Bool()))

  val reactorTags = Vec(c.reactors.length, Decoupled(TimeTag()))
  val reactorsBusy = Vec(c.reactors.length, Input(Bool()))

  val logicalTimeOut = Decoupled(TimeTag())
  val nextEvent = Decoupled(TimeTag())
  val waitForFinish = Flipped(Decoupled())

  val logicalTimeIn = Flipped(Decoupled(TimeTag()))
  val startScheduler = Decoupled()



  def tieOff(): Unit = {
    actionTags.map(_.ready := false.B)
    timerTags.map(_.ready := false.B)
    reactorTags.map(_.ready := false.B)
    logicalTimeOut.valid := false.B
    logicalTimeOut.bits := TimeTag(0.U)
  }

  def subElementsBusy(): Bool = {
    !(actionsBusy.reduce(_||_) || timersBusy.reduce(_||_) || reactorsBusy.reduce(_||_))
  }

  def nSubElements(): Int = {
    c.reactors.length + c.actions.length + c.timers.length
  }
}


/**
 *
 * ClockKeeper is in the Contained Reactors
 */
class ClockKeeper(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends AbstractClockKeeper {

  val io = IO(new ClockKeeperIO(c))
  io.tieOff()

  val logicalTime = RegInit(TimeTag(0.U))
  val physicalTime = RegInit(TimeTag(0.U))

  val execSchedule = if(c.getNumReactorLevels() > 0) Some(Module(new ExecutionSchedule(c))) else None
  val sorter = Module(new TreeSorter(TimeTag(), io.nSubElements())).io
  sorter.in.bits := TimeTag(0.U)
  sorter.in.valid := false.B


  val regNextEvent = RegInit(TimeTag(0.U))
  val regNextEventValid = RegInit(false.B)
  def connectSorter(): Unit = {
    for ((e,i) <- (io.actionTags ++ io.timerTags ++ io.reactorTags).zipWithIndex) {
      sorter.in.bits(i) := Mux(e.valid, e.bits, TimeTag(0.U))
      e.ready := sorter.in.ready
    }
    sorter.in.valid := (io.actionTags ++ io.timerTags ++ io.reactorTags).map(_.valid).reduce(_||_)
  }
  physicalTime.tag := physicalTime.tag + 1.U
  io.logicalTimeOut.bits := logicalTime

  io.nextEvent.bits := regNextEvent
  io.nextEvent.valid := regNextEventValid

  val sIdle :: sExec :: sBusy :: sSorting :: Nil = Enum(4)
  val regState = RegInit(sIdle)

  switch (regState) {
    is (sIdle) {
      io.logicalTimeIn.ready := true.B
      when (io.logicalTimeIn.fire) {
        logicalTime := io.logicalTimeIn.bits
        regState := sExec
        assert(!io.logicalTimeIn.bits.tag > regNextEvent.tag && regNextEventValid, "[ClockKeeper.scala] ClockKeeper received logical time which was greater than its next event")
      }
      assert(!io.subElementsBusy(), "[ClockKeeper.scala] ClockKeeper in Idle mode but subelements busy")
    }

    is (sExec) {
      io.logicalTimeOut.bits := logicalTime
      io.logicalTimeOut.valid := true.B
      when (io.logicalTimeOut.fire) {
        regState := sBusy
      }
      assert(io.logicalTimeOut.fire, "[ClockKeeper.scala] Entere sExec but sub-components not ready")

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




