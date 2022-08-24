package reactor.lib

import chisel3._
import chisel3.util._

// StreamMapWithStride applies a constant function on a certain amount of elements


class StreamMapWithStrideIO[T <: Data](gen: T)extends Bundle {
  val in = Flipped(Decoupled(gen))
  val out = Decoupled(gen)

  def tieOff = {
    in.ready := false.B
    out.valid := false.B
    out.bits := 0.U.asTypeOf(gen)
  }
}
class StreamMapWithStride[T <: Data](gen: T, stride: Int, fxn: Vec[T] =>T) extends Module {

  val io = IO(new StreamMapWithStrideIO(gen))
  io.tieOff

  val buffer = RegInit(VecInit(Seq.fill(stride)(0.U.asTypeOf(gen))))
  val regIdx = RegInit(0.U((log2Ceil(stride)+1).W))

  val sFilling :: sCompute :: Nil = Enum(2)
  val regState = RegInit(sFilling)

  switch(regState) {
    is (sFilling) {
      io.in.ready := true.B
      when (io.in.fire) {
        buffer(regIdx) := io.in.bits

        when (regIdx === (stride-1).U) {
          regState := sCompute
          regIdx := 0.U
        } otherwise {
          regIdx := regIdx + 1.U
        }
      }
    }

    is (sCompute) {
      io.out.bits := fxn(buffer)
      io.out.valid := true.B
      when (io.out.fire) {
        regState := sFilling
      }
    }
  }
}
