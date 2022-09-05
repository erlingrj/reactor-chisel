package reactor

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util.Valid
import org.scalatest.flatspec.AnyFlatSpec


object TestPortAccess {
  def expRead(c: PortOutIO[Data], clk: Clock, vals: Seq[Int]): Unit = {
    var addr = 0
    for (v <- vals) {
      while(!c.en.peekBoolean()) {
        clk.step()
      }
      c.addr.expect(addr.U)
      clk.step()
      c.data.poke(v.U)
      addr += 1
    }
    clk.step()
  }

  def expWrite(c: PortInIO[Data], clk: Clock, eVals: Seq[Int]): Unit = {
      var addr = 0
      for (v <- eVals) {
        while (!c.en.peekBoolean()) {
          clk.step()
        }
        c.addr.expect(addr.U)
        c.data.expect(v.U)
        addr += 1
        clk.step()
      }
    }
}
class TestRegPort extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "TestRegPort"

  def write(c: Port[UInt], addr: Int, data: Int): Unit = {
    c.io.in.en.poke(true.B)
    c.io.in.addr.poke(addr.U)
    c.io.in.data.poke(data.U)
    c.clock.step()
    c.io.in.en.poke(false.B)
    c.io.in.addr.poke(0.U)
    c.io.in.data.poke(0.U)
  }

  def read(c: Port[UInt], idx: Int, addr:Int, expect: Int) = {
    c.io.outs(idx).addr.poke(addr.U)
    c.io.outs(idx).en.poke(true.B)
    c.clock.step()
    c.io.outs(idx).data.expect(expect.U)
  }

  val c = PortConfig(nElems = 4, nReaders = 2, gen = UInt(8.W))

  it should "initialize" in {
    test(new RegPort(c)) {c =>
      c.io.outs.map(_.present.expect(false.B))
    }
  }

  it should "read and write" in {
    test(new RegPort(c)) { c =>
      write(c, 1,2)
      c.io.outs.map(_.present.expect(true.B))
      read(c,0,1,2)
    }
  }

}