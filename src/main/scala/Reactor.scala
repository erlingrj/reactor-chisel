package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror

import fpgatidbits.PlatformWrapper._
import chisel3.experimental.noPrefix


class ReactorIO extends Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val presentIn = Input(UInt(32.W))
    val presentOut = Output(UInt(32.W))
    val baseAddr = Input(UInt(64.W))
    val baseAddrRes = Input(UInt(64.W))
    val faultType = Output(UInt(8.W))
    val faultReaction = Output(UInt(16.W))
    val cycles = Output(UInt(32.W))
    val currReaction = Output(UInt(8.W))
    val signature = Output(UInt(32.W))

  def tieOff = {
    done := false.B
    faultType := ReactorFault.None.asUInt
    faultReaction := 0.U
    currReaction := 0.U
    presentOut := 0.U
  }
}

// TODO: Make static check of: nPorts (vs 32 is the max number of presence signals)
// TODO: require that the output ports only have 1 reader
abstract class ReactorBase(p: PlatformWrapperParams)
  extends GenericAccelerator(p) {

  val numMemPorts = 1
  val ioMem = IO(new GenericAcceleratorMemIO(numMemPorts, p))
  val io = IO(new ReactorIO())
  io.signature := makeDefaultSignature()
  io.tieOff

  val dma: ReactorDMA
  val scheduler: Scheduler
  val inPorts: Array[Port[_ <: Data]]
  val outPorts: Array[Port[_ <: Data]]
  val states: Array[ReactorState[_<:Data]]

  def connectScheduler2Ports = {
    (inPorts ++ outPorts).map(_.io.evalEnd := scheduler.ioSchedulerCtrl.done)
  }


  // Top-level state machine
  val sIdle :: sRead :: sRunning :: sWriteBack :: sDone :: Nil = Enum(5)
  val regState = RegInit(sIdle)
  val regCycles = RegInit(0.U(32.W))

  io.cycles := regCycles

  def reactorMain: Unit = {

    switch(regState) {
      is (sIdle) {
        when (io.start) {
          dma.io.readStart.valid := true.B
          dma.io.readStart.bits.baseAddr := io.baseAddr
          dma.io.readStart.bits.present := io.presentIn.asBools.take(inPorts.length)

          assert(io.presentIn > 0.U, "Top Reactor started with no present input signals")

          when (dma.io.readStart.fire) {
            regCycles := 0.U
            regState := sRead
          }
        }
      }

      is (sRead) {
        when(dma.io.readDone) {
          scheduler.ioSchedulerCtrl.start.valid := true.B
          assert(scheduler.ioSchedulerCtrl.start.fire, "[Reactor] Reactor enter running state but scheduler not ready")
          regState := sRunning
        }
      }

      is (sRunning) {
        when (scheduler.ioSchedulerCtrl.done) {
          val presents = VecInit(outPorts.map{_.io.outs(0).present}.toSeq)
          when(presents.reduce(_||_)) {
            dma.io.writeStart.valid := true.B
            dma.io.writeStart.bits.baseAddr := io.baseAddrRes
            dma.io.writeStart.bits.present := presents
            assert(dma.io.writeStart.fire)
            regState := sWriteBack
          } otherwise {
            regState := sDone
          }
        }
      }

      is (sWriteBack) {
        when (dma.io.writeDone) {
          regState := sDone
        }
      }

      is (sDone) {
        io.done := true.B
        when (io.start) {
          dma.io.readStart.valid := true.B
          dma.io.readStart.bits.baseAddr := io.baseAddr
          dma.io.readStart.bits.present := io.presentIn.asBools.take(inPorts.length)

          assert(io.presentIn > 0.U, "Top Reactor started with no present input signals")

          when (dma.io.readStart.fire) {
            regCycles := 0.U
            regState := sRead
          }
        }

      }
    }

    when (regState =/= sIdle) {
      regCycles:= regCycles + 1.U
    }
  }

}
