package reactor

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util.Valid
import org.scalatest.flatspec.AnyFlatSpec



class TestPort2 extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "TestPort2"
  val c = InputPortConfig(
    gen = new Event(gen = UInt(8.W), depth=8),
    nReaders = 8
  )

  it should "initialize" in {
    test(new InputPort(c)) {c =>
      c.io.outward.fire.expect(false.B)
    }
  }
}
