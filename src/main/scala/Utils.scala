package reactor

import chisel3._
import chisel3.util._


class TreeSorterIO[T <: Data](gen: T, n: Int) extends Bundle {
  val in = Flipped(Decoupled(Vec(n, gen)))
  val out = Decoupled(gen)

  def tieOff: Unit = {

  }
}

class TreeSorter[T <: Data](gen:T, n: Int) extends Module {
  val io = IO(new TreeSorterIO(gen, n))
  io.tieOff

  val sIdle :: sSort :: sDone :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  val regs = RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(gen).asUInt)))
  val cnt = RegInit(0.U(log2Ceil(n)))

  switch(regState) {
    is (sIdle) {
      io.in.ready := true.B
      when (io.in.fire) {
        regState := sSort
        regs := io.in.bits.map(_.asUInt)
        cnt := 0.U
      }
    }

    is (sSort) {
      for (i <- 0 until n/2) {
        regs(i) := Mux(regs(i*2) > regs(i*2+1), regs(i*2), regs(i*2+1))
      }
      cnt := cnt + 1.U
      when (cnt === (n-1).U) {
        regState := sDone
      }

      when (io.in.valid) {
        regState := sIdle
      }
    }

    is (sDone) {
      io.out.valid := true.B
      io.out.bits := regs(0).asTypeOf(gen)
      when (io.out.fire) {
        regState := sIdle
        cnt := 0.U
        regs.map(_:=0.U)
      }.elsewhen (io.in.valid) {
        regState := sIdle
      }
    }
  }
}