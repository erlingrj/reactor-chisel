package reactor

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.AddVecLiteralConstructor
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}

class TestTreeSorter extends FlatSpec with ChiselScalatestTester with Matchers {

  def initClocks(implicit c: TreeSorter[_ <: Data]): Unit = {
    c.io.in.initSource().setSourceClock(c.clock)
    c.io.out.initSink().setSinkClock(c.clock)
  }


  /**
   * Make my own version of "enqueue" to handle parameteric data and VecLiterals
   */
  def in(e: Seq[Int])(implicit c: TreeSorter[UInt]): Unit = {
    for (i <- 0 until e.length) {
      c.io.in.bits(i).poke(e(i).U)
    }
    c.io.in.valid.poke(true.B)
    fork
      .withRegion(Monitor) {
        while (c.io.in.ready.peek().litToBoolean == false) {
          c.clock.step()
        }
      }
      .joinAndStep(c.clock)
    c.io.in.valid.poke(false.B)
  }

  def out(e: Int)(implicit c: TreeSorter[UInt]): Unit = {
    c.io.out.ready.poke(true.B)
    fork
      .withRegion(Monitor) {
        c.io.out.waitForValid()
        c.io.out.valid.expect(true.B)
        c.io.out.bits.expect(e.U)
      }
      .joinAndStep(c.clock)
  }

  behavior of "TreeSorter"

  it should "Initialize correctly" in {
    test(new TreeSorter(gen = UInt(8.W), n = 8)) { implicit c =>
      initClocks
      c.io.in.ready.expect(true.B)
      c.io.out.valid.expect(false.B)
    }
  }

  it should "Sort simple" in {
    test(new TreeSorter(gen = UInt(8.W), n = 8)) { implicit c =>
      initClocks
      val e = Seq(4, 6, 2, 3, 1, 4, 9, 2)
      in(e)
      out(e.min)
    }
  }
  it should "Sort multiple simple" in {
    test(new TreeSorter(gen = UInt(8.W), n = 8)) { implicit c =>
      initClocks
      for (i <- 0 until 50) {
        val e = Seq.tabulate(8)(j => (j * i + 13) % 256)
        in(e)
        out(e.min)
      }
    }
  }
}
