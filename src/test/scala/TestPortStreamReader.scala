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
    while(!c.io.portRead.en.peekBoolean()) {
      c.clock.step()
    }
  }

  val gen = UInt(4.W)
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
}
