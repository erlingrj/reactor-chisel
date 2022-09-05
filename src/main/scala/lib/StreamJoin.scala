package reactor.lib

import chisel3._
import chisel3.util._

// StreamJoin applies a function to two input streams and creates an output stream
// E.g. Add
// In1=[1,2,3,2]
// In2=[3,4,5,4]
// def fxn(a: UInt, b: UInt): UInt = a+b
// Out=[4,6,8,6]

// TODO: Add tests for this library function
class StreamJoinIO[T1 <: Data, T2 <: Data, T3 <: Data](genIn1: T1, genIn2: T2, genOut: T3) extends Bundle {
  val in1 = Flipped(Decoupled(genIn1))
  val in2 = Flipped(Decoupled(genIn2))
  val out = Decoupled(genOut)

  def tieOff = {
    in1.ready := false.B
    in2.ready := false.B
    out.valid := false.B
    out.bits := 0.U.asTypeOf(genOut)
  }
}

class StreamJoin[T1 <: Data, T2 <: Data, T3 <: Data](genIn1: T1, genIn2: T2, genOut: T3, fxn: (T1,T2) => T3 ) extends Module {
  val io = IO(new StreamJoinIO(genIn1, genIn2, genOut))
  io.tieOff

  val q1 = Module(new Queue(genIn1, 1, pipe = true, flow = false)).io
  val q2 = Module(new Queue(genIn2, 1, pipe = true, flow = false)).io

  q1.enq <> io.in1
  q2.enq <> io.in2

  q1.deq.ready := false.B
  q2.deq.ready := false.B

  when(io.out.ready && q1.deq.valid && q2.deq.valid) {
    q1.deq.ready := true.B
    q2.deq.ready := true.B
    io.out.valid := true.B
    io.out.bits := fxn(q1.deq.bits, q2.deq.bits)
  }
}
