package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror

import fpgatidbits.PlatformWrapper._
import chisel3.experimental.noPrefix


class ReactorIO extends Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
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
  }
}

abstract class ReactorBase(p: PlatformWrapperParams)(implicit val rp: ReactorGlobalParams)
  extends GenericAccelerator(p) {

  //
  val numMemPorts = 1
  val ioMem = IO(new GenericAcceleratorMemIO(numMemPorts, p))
  val io = IO(new ReactorIO())
  io.signature := makeDefaultSignature()
  io.tieOff

  val dma: ReactorDMA[_<:Data, _<:Data]
  val scheduler: Scheduler
  val ports: Seq[_ <: Port[_ <: Data]]
  def pTopIn = ports(0)
  def pTopOut = ports.last

  def connectScheduler2Ports = {
    ports.map(_.io.evalEnd := scheduler.ioSchedulerCtrl.done)
  }


  def inByteCount: Int
  def outByteCount: Int

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
          dma.io.readStart.bits.byteCount := inByteCount.U

          when (dma.io.readStart.fire) {
            regCycles := 0.U
            regState := sRead
          }
        }
      }

      is (sRead) {
        when(dma.io.readDone) {
          assert(pTopIn.io.outs(0).present, "[Reactor] DMA finished but top input port not present")
          scheduler.ioSchedulerCtrl.start.valid := true.B
          assert(scheduler.ioSchedulerCtrl.start.fire, "[Reactor] Reactor enter running state but scheduler not ready")
          regState := sRunning
        }
      }

      is (sRunning) {
        when (scheduler.ioSchedulerCtrl.done) {

          when(pTopOut.io.outs(0).present) {
            dma.io.writeStart.valid := true.B
            dma.io.writeStart.bits.byteCount := outByteCount.U
            dma.io.writeStart.bits.baseAddr := io.baseAddrRes
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
        //TODO: Now we are stuck at sDone until we do a reset
        io.done := true.B
      }
    }

    when (regState =/= sIdle) {
      regCycles:= regCycles + 1.U
    }
  }

}
