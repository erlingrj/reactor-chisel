package reactor

import reactor.lib._

import chisel3._
import chisel3.util._

import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.PlatformWrapper.GenericAccelImplicits._

import TestPortAccess._


class TestVaddReactor extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "VaddReactor"

  it should "initialize" in {
    test(new TesterWrapper({p => new VaddReactor(p)}, "_test")) { c =>
      c.expectReg("cycles", 0.U)
    }
  }

  it should "perform addition" in {
    test(new TesterWrapper({ p => new VaddReactor(p, vLen = 5) }, "_test")) { c =>
      val baseAddr = 0
      val baseAddrRes = 512
      val present = Seq(true, true)
      val numIn1 = Seq(10,100,12,42,9)
      val numIn2 = Seq(54,78,60,2,4)
      val numOut = numIn1 zip numIn2 map {case (l,r) => l+r}
      c.arrayToMem(0, numIn1.map(_.U) ++ numIn2.map(_.U))
      c.start(baseAddr, baseAddrRes, present)
      c.waitForDone
      c.expectArrayMem(baseAddrRes, numOut)
    }
  }

  it should "perform multple additions" in {
    test(new TesterWrapper({ p => new VaddReactor(p, vLen = 5) }, "_test")) { c =>
      val baseAddr = 0
      val baseAddrRes = 512
      val numIns1 = Seq(
        Seq(10,100,12,42,9),
        Seq(14,0,1,22,93),
        Seq(96,10,6,21,32)
      )

      val numIns2= Seq(
        Seq( 54,78,60,2,4),
        Seq(64,8,6,12,78),
        Seq(9,67,65,22,13)
      )
      val present = Seq(true,true)

      for ((numIn1, numIn2) <- numIns1 zip numIns2) {
        val numOut = numIn1 zip numIn2 map {case(l,r) => l+r}
        println(numIn1)
        println(numIn2)
        c.arrayToMem(0, numIn1.map(_.U) ++ numIn2.map(_.U))
        c.start(baseAddr, baseAddrRes, present)
        c.waitForDone
        c.expectArrayMem(baseAddrRes, numOut)
        c.doReset()
      }

    }
  }

}
