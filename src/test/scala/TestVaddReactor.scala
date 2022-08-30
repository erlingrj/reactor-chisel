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

  behavior of "VaddSimpleReactor"

  it should "initialize" in {
    test(new TesterWrapper({p => new VaddReactor(p)}, "_test")) { c =>
      c.expectReg("cycles", 0.U)
    }

  }

  it should "perform addition" in {
    test(new TesterWrapper({ p => new VaddReactor(p) }, "_test")) { c =>
      val baseAddr = 0
      val baseAddrRes = 512
      val numIn = Seq(10,100,12,42,9, 54,78,60,2,4)
      val numOut = numIn.sliding(2,2).map(_.sum).toSeq
      c.arrayToMem(0, numIn.map(_.U))
      c.start(baseAddr, baseAddrRes)
      c.waitForDone
      c.expectArrayMem(baseAddrRes, numOut)
    }
  }

  it should "perform multple additions" in {
    test(new TesterWrapper({ p => new VaddReactor(p) }, "_test")) { c =>
      val baseAddr = 0
      val baseAddrRes = 512
      val numIns = Seq(
        Seq(10,100,12,42,9, 54,78,60,2,4),
        Seq(14,0,1,22,93, 64,8,6,12,78),
        Seq(96,10,6,21,32, 9,67,65,22,13)
      )

      for (numIn <- numIns) {
        val numOut = numIn.sliding(2,2).map(_.sum).toSeq
        c.arrayToMem(0, numIn.map(_.U))
        c.start(baseAddr, baseAddrRes)
        c.waitForDone
        c.expectArrayMem(baseAddrRes, numOut)
        c.doReset()
      }

    }
  }

}
