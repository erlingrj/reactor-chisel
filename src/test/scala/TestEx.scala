package reactor

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._

class ExampleIO[T<: Data](gen: T) extends Bundle {
  val data = Input(gen)
}

abstract class AbstractExample extends Module {
  val io: ExampleIO[_<:Data]
}

class Example extends AbstractExample {
  override val io = IO(new ExampleIO(UInt(8.W)))
}

class TestEx extends FlatSpec with ChiselScalatestTester with Matchers {

  def pokeAbstract(c: AbstractExample): Unit = {
  //  c.io.data.poke(2.U.asTypeOf(c.io.data))
  }

  behavior of "Example"


  it should "work" in {
    test(new Example) { c =>
//      pokeAbstract(c)
    }
  }
}


class Ex2 extends Module {

  val resetSignal = Wire(Bool())
  resetSignal := false.B

  val done = Wire(Bool())
  done := false.B

  def someSequentialLogic: Unit = {
    withReset(resetSignal) {
      val regIdx = RegInit(0.U)

      when (regIdx < 10.U) {
        regIdx := regIdx + 1.U
      }.otherwise {
        regIdx := 0.U
      }
    }
  }
  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  switch(regState) {
    is (sIdle) {
      regState := sRunning
    }

    is (sRunning) {
      someSequentialLogic

      when (done) {
        resetSignal := true.B
        regState := sDone
      }
    }

    is (sDone) {

    }
  }
}