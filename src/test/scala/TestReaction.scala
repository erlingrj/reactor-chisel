package reactor.test

import reactor._
import reactor.examples.ReactionAddN

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.VecLiterals._



//class TestReaction extends AnyFlatSpec with ChiselScalatestTester {
//  behavior of "R1"
//  it should "initialize" in {
//    test(new ReactionAddN(1)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
//      implicit val clk = c.clock
//      clk.step(5)
//      c.in.fire.expect(false.B)
//      c.out.req.valid.expect(false.B)
//    }
//  }
//
//  it should "fire and consume" in {
//    test(new ReactionAddN(1)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
//      implicit val clk = c.clock
//      clk.step(5)
//      fork {
//        ReactorSim.readSlave(c.io.in, 13.U)
//      }.fork {
//        ReactorSim.writeSlave(c.io.out, 14.U)
//      }.join()
//    }
//  }
//}
