package reactor

import reactor.lib._
import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec

import TestPortAccess._
import TestReactionDriver._

class TestVadd2Reaction extends AnyFlatSpec with ChiselScalatestTester {

  def initClocks(c: Reaction): Unit = {
    c.ioCtrl.enable.initSource().setSourceClock(c.clock)
  }

  val rc = ReactorGlobalParams()

  behavior of "Vadd2Reaction"

  it should "initialize" in {
    test(new VaddStreamReaction(10)(rc)) { c =>
      initClocks(c)
      c.out.en.expect(false.B)
      c.in.addr.expect(0.U)
    }
  }

  it should "read from port" in {
    test(new VaddStreamReaction(10)(rc)) { c =>
      initClocks(c)
      val in = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val out = in.sliding(2,2).map(_.sum).toSeq
      start(c, c.clock)
      expRead(c.in, c.clock, in)
    }
  }

  it should "do computation" in {
    test(new VaddStreamReaction(10)(rc)) { c =>
      initClocks(c)
      val in = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val out = in.sliding(2,2).map(_.sum).toSeq
      start(c, c.clock)
      fork {
        expRead(c.in, c.clock, in)
      }.fork {
        expWrite(c.out, c.clock, out)
      }.join()
    }
  }
}