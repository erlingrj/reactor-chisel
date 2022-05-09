package reactor

import chisel3._
import chisel3.util._

/**
 *  Ports are what connects Reactors to Reactor
 *  OutputPort
 */



class OutPortIO[T <: Data](c: ReactorOutputPortConfig[T])(implicit rc: ReactorGlobalParams) extends Bundle {
  val write = Vec(c.numAntiDependencies, Flipped(Decoupled(Signal(c.gen))))
  val push = Input(Bool())

  val out = Decoupled(Signal(c.gen))
  def tieOff() = {
    out.valid := false.B
    out.bits := 0.U.asTypeOf(Signal(c.gen))
    write.map(_.ready := false.B)
  }
}

class OutPort[T <: Data](c: ReactorOutputPortConfig[T])(implicit rc: ReactorGlobalParams) extends Module {
  val io = IO(new OutPortIO(c))
  io.tieOff()

  val regBuf = RegInit(0.U.asTypeOf(Signal(c.gen)))
  val regBufVal = RegInit(false.B)

  val sRecv :: sSend :: Nil = Enum(2)

  val regState = RegInit(sRecv)

  switch(regState) {
    is (sRecv) {

      for (i <- 0 until c.numAntiDependencies) {
        io.write(i).ready := true.B
        when (io.write(i).fire) {
          regBufVal := true.B
          regBuf := io.write(i).bits
        }
      }
      assert(io.write.count(_.fire) <= 1.U, "[Port.scala] Multiple write to PortOut in same clock cycle. Should be mutex")

      when(io.push) {
        regState := sSend
      }
    }

    is (sSend) {
      io.out.bits := regBuf
      io.out.valid := regBufVal

      when (io.out.fire) {
        regState := sRecv
        regBufVal := false.B
      }
    }
  }
}


class InPortIO[T <: Data](c: ReactorInputPortConfig[T])(implicit rc: ReactorGlobalParams) extends Bundle {
  val in = Flipped(Decoupled(Signal(c.gen)))
  val read = Decoupled(Signal(c.gen))

  def tieOff() = {
    in.ready := false.B
    read.valid := false.B
    read.bits := 0.U.asTypeOf(Signal(c.gen))
  }
}


class InPort[T <: Data](c: ReactorInputPortConfig[T])(implicit rc: ReactorGlobalParams) extends Module {

  val io = IO(new InPortIO(c))
  io.tieOff()

  val regBuf = RegInit(0.U.asTypeOf(Signal(c.gen)))
  val regBufVal = RegInit(false.B)


  val sRecv :: sSend :: Nil = Enum(2)
  val regState = RegInit(sRecv)
  val regFired = RegInit(0.U(log2Ceil(c.numDependencies+1).W))

  switch (regState) {

    is (sRecv) {
      io.in.ready := true.B
      when (io.in.fire) {
        regBuf := io.in.bits
        regBufVal := true.B
        regState := sSend
      }
    }

    is (sSend) {
      io.read.bits := regBuf
      io.read.valid := true.B

      when(io.read.fire) {
        regFired := regFired + 1.U
        when (regFired === (c.numDependencies-1).U) {
          regBuf := 0.U.asTypeOf(Signal(c.gen))
          regBufVal := false.B
          regState := sRecv
          regFired := 0.U
        }

      }
    }
  }
}