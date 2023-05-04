package reactor.test

import reactor.examples._
import reactor._
import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec



class TestReactor extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Reactor1"
  it should "initialize" in {
    test(new DualAddN) { c =>
      implicit val clk = c.clock
    }
  }
  it should "Respect precedence" in {
    test(new DualAddN).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val clk = c.clock
      fork {
        ReactorSim.readSlave(c.io.in(0), 42.U)
      }.fork.withRegion(Monitor) {
        ReactorSim.writeSlave(c.io.out, 43.U, fire=false)
        ReactorSim.writeSlave(c.io.out, 44.U)
      }.joinAndStep(clk)

    }
  }

  behavior of "Reactor2"
  it should "initialize" in {
    test(new DualWithContained) { c =>
      implicit val clk = c.clock
    }
  }

  it should "Respect precedence" in {
    test(new DualWithContained).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val clk = c.clock
      fork {
        ReactorSim.readSlave(c.io.in(0), 42.U)
      }.fork {
        ReactorSim.readSlave(c.io.in(1), 42.U)
      }.fork.withRegion(Monitor) {
        ReactorSim.writeSlave(c.io.out1, 45.U, fire = false)
        ReactorSim.writeSlave(c.io.out1, 46.U)
      }.fork.withRegion(Monitor) {
        ReactorSim.writeSlave(c.io.out2, 43.U, fire = false)
        ReactorSim.writeSlave(c.io.out2, 44.U)
      }.joinAndStep(clk)
    }
  }
  behavior of "Reactor3"
  it should "initialize" in {
    test(new DualDataflow) { c =>
      implicit val clk = c.clock
    }
  }

  it should "Respect precedence" in {
    test(new DualDataflow).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val clk = c.clock
      fork {
        ReactorSim.readSlave(c.io.in(0), 42.U)
      }.fork {
        ReactorSim.readSlave(c.io.in(1), 42.U)
      }.fork.withRegion(Monitor) {
        ReactorSim.writeSlave(c.io.out1, 47.U, fire=false)
        ReactorSim.writeSlave(c.io.out1, 48.U)
      }.fork.withRegion(Monitor) {
        ReactorSim.writeSlave(c.io.out2, 45.U, fire=false)
        ReactorSim.writeSlave(c.io.out2, 46.U)
      }.joinAndStep(clk)
    }
  }
}
