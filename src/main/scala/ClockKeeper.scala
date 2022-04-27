package reactor

import chisel3._
import chisel3.util._




abstract class AbstractClockKeeper extends Module {

}

class TopClockKeeper(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends AbstractClockKeeper {

}

class ClockKeeperIO(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Bundle {

  val actionTags = Vec(c.actions.length, Decoupled(Tag()))
  val actionsBusy = Vec(c.actions.length, Input(Bool()))

  val timerTags = Vec(c.timers.length, Decoupled(Tag()))
  val timersBusy = Vec(c.timers.length, Input(Bool()))

  val reactorTags = Vec(c.reactors.length, Decoupled(Tag()))
  val reactorsBusy = Vec(c.reactors.length, Input(Bool()))

  val logicalTimeOut = Decoupled(Tag())
  val nextEvent = Decoupled(Tag())
  val busy = Output(Bool())

  val logicalTimeIn = Flipped(Decoupled(Tag()))
  val startScheduler = Decoupled(UInt(0.W))



  def tieOff(): Unit = {
    actionTags.map(_.ready := false.B)
    timerTags.map(_.ready := false.B)
    reactorTags.map(_.ready := false.B)
    logicalTimeOut.valid := false.B
    logicalTimeOut.bits := Tag(0.U)
  }

  def subElementsBusy(): Bool = {
    !(actionsBusy.reduce(_||_) || timersBusy.reduce(_||_) || reactorsBusy.reduce(_||_))
  }

  def nSubElements(): Int = {
    c.reactors.length + c.actions.length + c.timers.length
  }
}


class ClockKeeper(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends AbstractClockKeeper {

  val io = IO(new ClockKeeperIO(c))
  io.tieOff()

  val logicalTime = RegInit(Tag(0.U))
  val physicalTime = RegInit(Tag(0.U))

  val execSchedule = if(c.getNumReactorLevels() > 0) Some(Module(new ExecutionSchedule(c))) else None
  val sorter = Module(new TreeSorter(Tag(), io.nSubElements())).io

  def connectSorter(): Unit = {
    for ((e,i) <- (io.actionTags ++ io.timerTags ++ io.reactorTags).zipWithIndex) {
      sorter.in.bits(i) := Mux(e.valid, e.bits, Tag(0.U))
    }
  }
  physicalTime.tag := physicalTime.tag + 1.U
  io.logicalTimeOut.bits := logicalTime

  val sIdle :: sExec :: sSorting :: sBusy :: Nil = Enum(4)
  val regState = RegInit(sIdle)

  switch (regState) {
    is (sIdle) {
      io.logicalTimeIn.ready := true.B
      when (io.logicalTimeIn.fire) {
        logicalTime := io.logicalTimeIn.bits
        regState := sExec
      }.elsewhen(io.subElementsBusy()) {
        regState := sBusy
      }
    }

    is (sExec) {
      io.logicalTimeOut.bits := logicalTime
      io.logicalTimeOut.valid := true.B
      when (io.logicalTimeOut.fire) {
        regState := sIdle
      }

    }

    is (sSorting) {

    }

    is (sBusy) {

    }
  }


  when ()


  when(!io.subElementsBusy()) {


  }



}




