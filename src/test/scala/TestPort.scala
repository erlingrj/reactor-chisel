package reactor.test

import reactor._

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import reactor.test.ReactorSim

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

  it should "work with multiple readers" in {
    test(new InputPort(
      InputPortConfig(
        gen = new SingleToken(gen = UInt(8.W)),
        nReaders = 2
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) {c =>
      implicit val clk = c.clock
      fork {
        ReactorSim.readSlave(c.io.outward, 23.U)
      }.fork.withRegion(Monitor) {
        ReactorSim.readMaster(c.io.inward(0), 23.U)
        c.io.inward(0).resp.valid.expect(false.B)
        c.io.inward(0).resp.present.expect(false.B)
        ReactorSim.readMaster(c.io.inward(1), 23.U)
        c.io.inward(1).resp.valid.expect(false.B)
        c.io.inward(1).resp.present.expect(false.B)
      }.joinAndStep(clk)
    }
  }

  behavior of "SingleValueEvent OutputPort"
  it should "initialize" in {
    test(new OutputPort(
      OutputPortConfig(
        gen = new SingleToken(gen = UInt(8.W)),
        nWriters = 1
      )
    )) { c =>
      c.io.inward(0).req.valid.poke(true.B)
      c.io.outward.req.valid.expect(true.B)
    }
  }

  it should "work with multiple writers" in {
    test(new OutputPort(
      OutputPortConfig(
        gen = new SingleToken(gen = UInt(8.W)),
        nWriters = 2
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val clk = c.clock
      fork {
        ReactorSim.writeMaster(c.io.inward(0), 12.U)
        ReactorSim.writeMaster(c.io.inward(1), 14.U)
      }.fork.withRegion(Monitor) {
        ReactorSim.writeSlave(c.io.outward, 12.U, fire = false)
        ReactorSim.writeSlave(c.io.outward, 14.U)

      }.joinAndStep(clk)
    }
  }
}
