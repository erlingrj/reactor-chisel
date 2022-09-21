package reactor

import chisel3._
import chiseltest._
import fpgatidbits.PlatformWrapper.GenericAccelImplicits._
import fpgatidbits.PlatformWrapper._
import org.scalatest.flatspec.AnyFlatSpec
import reactor.examples.StatefulReactor

import scala.collection.mutable.ArrayBuffer



class TestStatefulReactor extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "StatefulReactor"

  it should "initialize" in {
    test(new TesterWrapper({p => new StatefulReactor(p)}, "_test")) { c =>
      c.expectReg("cycles", 0.U)
    }
  }

  it should "perform addition" in {
    test(new TesterWrapper({ p => new StatefulReactor(p) }, "_test")) { c =>
      val baseAddr = 0
      val baseAddrRes = 512
      val present = Seq(true, true)
      val numIn1 = Seq(1)
      val numIn2 = Seq(4)
      val numOut = Seq(5)
      c.arrayToMem(0, numIn1.map(_.U) ++ numIn2.map(_.U))
      c.start(baseAddr, baseAddrRes, present)
      c.waitForDone
      c.expectArrayMem(baseAddrRes, numOut.toSeq)
    }
  }

  it should "perform multple additions" in {
    test(new TesterWrapper({ p => new StatefulReactor(p) }, "_test")) { c =>
      val baseAddr = 0
      val baseAddrRes = 512
      val numIns1 = Seq(1,10,12,4,9)
      val numIns2= Seq( 5,7,6,2,4)
      val present = Seq(true,true)

      var run = 0
      for ((numIn1, numIn2) <- numIns1 zip numIns2) {
        run += numIn1 + numIn2
        val numOut = run
        c.arrayToMem(0, Seq(numIn1.U, numIn2.U))
        c.start(baseAddr, baseAddrRes, present)
        c.waitForDone
        c.expectArrayMem(baseAddrRes, Seq(numOut))
      }
    }
  }
}
