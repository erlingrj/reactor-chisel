//package reactor
//
//import reactor.lib._
//import chisel3._
//import org.scalatest._
//import chiseltest._
//import chisel3.experimental.BundleLiterals._
//import chisel3.util._
//import org.scalatest.flatspec.AnyFlatSpec
//
//object TestReactionDriver {
//  def start(c: Reaction, clk: Clock) = {
//    c.ioCtrl.enable.enqueueNow(chiselTypeOf(c.ioCtrl.enable.bits))
//    c.ioCtrl.running.expect(true.B)
//  }
//}
