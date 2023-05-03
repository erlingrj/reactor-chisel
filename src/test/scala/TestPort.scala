package reactor

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec

class TestPort extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "PureEvent InputPort"
  it should "initialize" in {
    test(new InputPort(
      InputPortConfig(
        gen = new PureToken,
        nReaders = 1
      )
    )) { c =>
      c.io.outward.resp.valid.poke(true.B)
      c.io.inward(0).resp.valid.expect(true.B)
    }
  }

  behavior of "SingleValueEvent InputPort"
  it should "initialize" in {
    test(new InputPort(
      InputPortConfig(
        gen = new SingleToken(gen = UInt(8.W)),
        nReaders = 1
      )
    )) { c =>
      c.io.outward.resp.valid.poke(true.B)
      c.io.inward(0).resp.valid.expect(true.B)
    }
  }
}
