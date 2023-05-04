package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.VecLiterals._
import chisel3.experimental.{DataMirror, Direction}


case class SchedulerConfig(
                          nTopInputPorts: Int = 0,
                          nTopOutputPorts: Int = 0
                          )
class SchedulerIO(c: SchedulerConfig) extends Bundle {
  val start = Input(Bool())
  val done = Output(Bool())
  val running = Output(Bool())
  val fireOut = Vec(c.nTopOutputPorts, Input(Bool()))
  val fireIn = Vec(c.nTopInputPorts, Output(Bool()))

  def driveDefaults() = {
    done := false.B
    running := false.B
    fireIn.foreach(_ := false.B)
  }
}

class Scheduler(c: SchedulerConfig) extends Module {
  val io = IO(new SchedulerIO(c))
  io.driveDefaults()

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regState = RegInit(sIdle)
  val regFired = RegInit(VecInit(Seq.fill(c.nTopOutputPorts)(false.B)))

  val regDone = RegInit(false.B)
  io.done := regDone
  io.running := regState === sRunning

  switch(regState) {
    is (sIdle) {
      when (io.start) {
        io.fireIn.foreach(_:=true.B)
        regState := sRunning
        regDone := false.B
      }
    }
    is (sRunning) {
      for (i <- 0 until c.nTopOutputPorts) {
        when (io.fireOut(i)) {
          regFired(i) := true.B
        }
      }

      when (util.PopCount(regFired) === c.nTopOutputPorts.U) {
        regState := sDone
        regDone := true.B
      }
    }

    is (sDone) {
      regFired.foreach(_:=false.B)
      regState := sIdle
    }
  }

  var outputIdx = 0
  var inputIdx = 0
  def connect(in: TopInputPort[_,_]): Unit = {
    require(inputIdx < io.fireOut.length)
    in.io.fire := io.fireIn(inputIdx)
    inputIdx += 1
  }

  def connect(out: TopOutputPort[_,_]): Unit = {
    require(outputIdx < io.fireOut.length)
    io.fireOut(outputIdx) := out.io.fire
    outputIdx += 1
  }
}
