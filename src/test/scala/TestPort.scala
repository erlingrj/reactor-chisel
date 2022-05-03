package reactor

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import org.scalatest._

class TestPorts extends FlatSpec with ChiselScalatestTester with Matchers {

  def initClocks(c: InPort[_ <: Data]): Unit = {
    c.io.in.initSource().setSourceClock(c.clock)
    c.io.read.initSink().setSinkClock(c.clock)
  }

  def initClocks(c: OutPort[_ <: Data]): Unit = {
    c.io.write.map(_.initSource().setSourceClock(c.clock))
    c.io.out.initSink().setSinkClock(c.clock)
  }

  implicit val rc = ReactorGlobalParams(numClkBits = 8)
  behavior of "InPort"

  val config1 = ReactorInputPortConfig(
    id = "test",
    numDependencies = 4,
    gen = UInt(8.W)
  )

  it should "Initialize correctly" in {
    test(new InPort(config1)) { c =>
      initClocks(c)
      c.io.read.valid.expect(false.B)
      c.io.in.ready.expect(true.B)
    }
  }
  it should "Forward data" in {
    test(new InPort(config1)) { c =>
      initClocks(c)
      c.io.in.enqueueNow(chiselTypeOf(c.io.in).bits.Lit(_.value -> 3.U))

      for (i <- 0 until config1.numDependencies) {
        c.io.in.ready.expect(false.B)
        c.io.read.expectDequeueNow(chiselTypeOf(c.io.read).bits.Lit(_.value -> 3.U))
      }
      c.io.in.ready.expect(true.B)
    }
  }

  it should "Forward multiple data" in {
    test(new InPort(config1)) { c =>
      initClocks(c)

      for (j <- 0 until 3) {
        c.io.in.enqueueNow(chiselTypeOf(c.io.in).bits.Lit(_.value -> j.U))

        for (i <- 0 until config1.numDependencies) {
          c.io.in.ready.expect(false.B)
          c.io.read.expectDequeueNow(chiselTypeOf(c.io.read).bits.Lit(_.value -> j.U))
        }
        c.io.in.ready.expect(true.B)
      }
    }
  }
/******************************************************************/
  behavior of "OutPort"

  val config2 = ReactorOutputPortConfig(
    id = "test",
    numAntiDependencies = 4,
    gen = UInt(8.W)
  )

  it should "Initialize correctly" in {
    test(new OutPort(config2)) { c =>
      initClocks(c)
      c.io.out.valid.expect(false.B)
      c.io.write.map(_.ready.expect(true.B))
    }
  }

  it should "Forward data" in {
    test(new OutPort(config2)) { c =>
      initClocks(c)
      for (i <- 0 until config2.numAntiDependencies) {
        c.io.write(i).enqueueNow(chiselTypeOf(c.io.write(i)).bits.Lit(_.value -> i.U))
      }
      c.io.out.valid.expect(false.B)
      c.io.push.poke(true.B)
      c.io.out.expectDequeue(chiselTypeOf(c.io.out).bits.Lit(_.value -> (config2.numAntiDependencies-1).U))
      c.io.push.poke(false.B)

      c.io.out.valid.expect(false.B)
      c.io.write.map(_.ready.expect(true.B))
    }
  }

  it should "Forward multiple data" in {
    test(new OutPort(config2)) { c =>
      initClocks(c)
      for (j <- 0 until 3) {
        for (i <- 0 until config2.numAntiDependencies) {
          c.io.write(i).enqueueNow(chiselTypeOf(c.io.write(i)).bits.Lit(_.value -> (j*config2.numAntiDependencies+i).U))
        }
        c.io.out.valid.expect(false.B)
        c.io.push.poke(true.B)
        c.io.out.expectDequeue(chiselTypeOf(c.io.out).bits.Lit(_.value -> (config2.numAntiDependencies*(j+1) - 1).U))
        c.io.push.poke(false.B)
        c.io.out.valid.expect(false.B)
      }
    }
  }
}
