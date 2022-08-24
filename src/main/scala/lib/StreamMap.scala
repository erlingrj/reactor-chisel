package reactor.lib

import chisel3._
import chisel3.util._

// StreamMap applies a constant function to each element of the stream
// E.g. Add2
// In=[1,2,3,2] out=[3,4,5,4]


class StreamMapIO[T <: Data](gen: T) extends Bundle {
  val in = Flipped(Decoupled(gen))
  val out = Decoupled(gen)
}

class StreamMap[T <: Data](gen: T, fxn: (T => T)) extends Module {
  val io = IO(new StreamMapIO(gen))

  val q = Module(new Queue(gen, 1, pipe = true, flow = false))

  q.io.enq <> io.in

  when(io.out.ready) {
    q.io.deq.ready := true.B
    when (q.io.deq.fire) {
      io.out.valid := true.B
      io.out.bits := fxn(q.io.deq.bits)
    }
  }
}
