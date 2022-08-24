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

  val rc = ReactorGlobalParams(devel = true)
  it should "initialize" in {
    test(new TesterWrapper({p => new VaddReactor(p)(rc)}, "_test")) { c =>
      c.expectReg("cycles", 0.U)
    }

  }

  it should "perform addition" in {
    test(new TesterWrapper({ p => new VaddReactor(p)(rc) }, "_test")) { c =>
      val baseAddr = 0
      val baseAddrRes = 512
      val numIn = Seq(10,100,12,42,9, 54,78,60,2,4)
      val numOut = numIn.sliding(2).map(_.sum)
      c.arrayToMem(0, numIn.map(_.U))
      c.start(baseAddr, baseAddrRes)
      c.waitForDone

    }
  }

}
