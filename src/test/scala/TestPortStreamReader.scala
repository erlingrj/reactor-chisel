package reactor

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec


class TestPortStreamReader extends AnyFlatSpec with ChiselScalatestTester {

  def initClocks(c: PortStreamReader[UInt]): Unit = {
    c.io.start.initSource().setSourceClock(c.clock)
    c.io.out.initSink().setSinkClock(c.clock)
  }

  def start(c: PortStreamReader[UInt]): Unit = {
    c.io.start.enqueueNow(chiselTypeOf(c.io.start.bits))
  }

  def expRead(c: PortStreamReader[UInt], vals: Seq[Int]): Unit = {
    var addr = 0
    for (v <- vals) {
      while(!c.io.portRead.en.peekBoolean()) {
        c.clock.step()
      }
      c.io.portRead.addr.expect(addr.U)
      c.clock.step()
      c.io.portRead.data.poke(v.U)
      addr += 1
    }
    c.clock.step()
  }


  def expOut(c: PortStreamReader[UInt], vals: Seq[Int]): Unit = {
    c.io.out.expectDequeueSeq(vals.map(_.U))
  }

  val gen = UInt(8.W)
  val c = PortIOConfig(nElems = 4)

  behavior of "PortStreamReader"

  it should "initialize" in {
    test(new PortStreamReader(c,gen)) {c =>
      initClocks(c)
      c.io.start.ready.expect(true.B)
      c.io.portRead.en.expect(false.B)
      c.io.out.valid.expect(false.B)
    }
  }

  it should "read and be done" in {
    test(new PortStreamReader(c, gen)) { c =>
      initClocks(c)
      start(c)
      val vals = Seq(1,2,3,4)
      fork {
        expRead(c, vals)
      }.fork {
        expOut(c,vals)
      }.join()

      c.io.done.expect(true.B)
      c.clock.step()
      c.io.start.ready.expect(true.B)
    }
  }

  val c2 = PortIOConfig(nElems = 1)
  it should "single entry" in {
    test(new PortStreamReader(c2, gen)) { c =>
      initClocks(c)
      start(c)
      val vals = Seq(9)
      fork {
        expRead(c, vals)
      }.fork {
        expOut(c,vals)
      }.join()

      c.io.done.expect(true.B)
      c.clock.step()
      c.io.start.ready.expect(true.B)
    }
  }
  it should "read multiple" in {
    test(new PortStreamReader(c, gen)) { c =>
      initClocks(c)
      for (i <- 0 until 10) {
        start(c)
        val vals = Seq(1,2,3,4)
        fork {
          expRead(c, vals)
        }.fork {
          expOut(c,vals)
        }.join()

        c.io.done.expect(true.B)
        c.clock.step()
        c.io.start.ready.expect(true.B)
      }
    }
  }
}
