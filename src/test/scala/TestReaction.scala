package reactor

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util.Valid

class AddReaction extends Reaction("TestReaction", TestPorts) {

  // Port references must be lazy so they dont reference and uninitialized field.s
  lazy val out: Valid[Data] = io.get("out")
  lazy val in1: Valid[Data] = io.get("in1")
  lazy val in2: Valid[Data] = io.get("in2")

  def reaction: Bool = {
    val done = WireInit(false.B)

    out.bits := Mux(in1.valid, in1.bits.asUInt, 0.U) + Mux(in2.valid, in2.bits.asUInt, 0.U)
    out.valid := true.B
    done := true.B

    done
  }
}

class TestReaction extends FlatSpec with ChiselScalatestTester with Matchers {

  def initClocks(c: Reaction): Unit = {
    c.io.inPorts.map(_.initSource().setSourceClock(c.clock))
    c.io.outPorts.map(_.initSink().setSinkClock(c.clock))
  }


  behavior of "TestReaction1"

  it should "work" in {

    test(new AddReaction) {c =>
      initClocks(c)
      c.io.enable.poke(true.B)
      c.in1.bits.poke(5.U)
      c.in2.bits.poke(10.U)

      c.clock.step()

      c.in1.valid.poke(true.B)
      c.in2.valid.poke(true.B)

      c.clock.step()
      // Enter running state


      c.out.expectDequeueNow(15.U)

    }


  }

}
