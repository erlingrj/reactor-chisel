package reactor

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec


class TestPortStreamWriter extends AnyFlatSpec with ChiselScalatestTester {

  def initClocks(c: PortStreamWriter[UInt]): Unit = {
    c.io.in.initSource().setSourceClock(c.clock)
  }

  def expWrite(c: PortStreamWriter[UInt], vals: Seq[Int]): Unit = {
    var addr = 0
    for (v <- vals) {
      while (!c.io.portWrite.en.peekBoolean()) {
        c.clock.step()
      }
      c.io.portWrite.addr.expect(addr.U)
      c.io.portWrite.data.expect(v.U)
      addr += 1
      c.clock.step()
    }
  }


  def enqIn(c: PortStreamWriter[UInt], vals: Seq[Int]): Unit = {
    c.io.in.enqueueSeq(vals.map(_.U))
  }

  val gen = UInt(8.W)
  val c = PortIOConfig(nElems = 4)
  val c2 = PortIOConfig(nElems = 1)

  behavior of "PortStreamWriter"

  it should "initialize" in {
    test(new PortStreamWriter(c, gen)) { c =>
      initClocks(c)
      c.io.portWrite.en.expect(false.B)
      c.io.in.ready.expect(true.B)
      c.io.done.expect(false.B)
    }
  }

  it should "write stream" in {
    test(new PortStreamWriter(c,gen)) {c =>
      initClocks(c)

      val vals = Seq(12,7,3,3)

      fork {
        enqIn(c,vals)
      }.fork {
        expWrite(c,vals)
      }.join()
      c.io.done.expect(true.B)
    }
  }

  it should "write multple streams" in {
    test(new PortStreamWriter(c,gen)) {c =>
      initClocks(c)
      val valss = Seq(Seq(22,13,23,4), Seq(56,23,1,0), Seq(99,2,23,12), Seq(12,7,3,3))
      for (vals <- valss) {

        fork {
          enqIn(c,vals)
        }.fork {
          expWrite(c,vals)
        }.join()
        c.io.done.expect(true.B)
      }
    }
  }

  it should "write single value stream" in {

    test(new PortStreamWriter(c2,gen)) {c =>
      initClocks(c)

      val vals = Seq(127)

      fork {
        enqIn(c,vals)
      }.fork {
        expWrite(c,vals)
      }.join()
      c.io.done.expect(true.B)
    }
  }
}
