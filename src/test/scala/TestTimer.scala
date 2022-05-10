package reactor

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}

class TestTimer extends FlatSpec with ChiselScalatestTester with Matchers {

  def initClocks(implicit c: Timer): Unit = {
    c.io.nextEvent.initSink().setSinkClock(c.clock)
    c.io.outPort.initSink().setSinkClock(c.clock)
    c.io.logicalTime.initSource().setSourceClock(c.clock)
  }


  def logicalTime(e: Int)(implicit c: Timer): Unit = {
    c.io.logicalTime.enqueue(chiselTypeOf(c.io.logicalTime).bits.Lit(
      _.tag -> e.U
    ))
  }

  def nextEvent(e: Int)(implicit c: Timer): Unit = {
    c.io.nextEvent.expectDequeue(chiselTypeOf(c.io.nextEvent).bits.Lit(
      _.tag -> e.U
    ))
  }

  def outPort(implicit c: Timer): Unit = {
    c.io.outPort.expectDequeue(chiselTypeOf(c.io.outPort.bits))
  }

  implicit val rc = ReactorGlobalParams(numClkBits = 8)

  behavior of "Timer"

  implicit val config1 = TimerConfig(
    id = "test",
    offset = 10,
    interval = 5,
    out = TimerPortConfig(id = "test_2"),
    numAntiDependencies = 3
  )

  it should "Initialize correctly" in {
    test(new Timer(config1)) { implicit c =>
      initClocks
      c.io.logicalTime.ready.expect(true.B)
      c.io.outPort.valid.expect(false.B)
      c.io.nextEvent.valid.expect(true.B)
    }
  }

  it should "Generate trigger" in {
    test(new Timer(config1)) { implicit c =>
      initClocks
      logicalTime(10)
      (1 to config1.numAntiDependencies).foreach(_ => outPort)
    }
  }
  it should "Generate multiple triggers" in {
    test(new Timer(config1)) { implicit c =>
      initClocks
      var time = config1.offset
      (1 to 10).foreach(_ => {
        logicalTime(time)
        (1 to config1.numAntiDependencies).foreach(_ => outPort)
        time += config1.interval
      })
    }
  }

  it should "Wrap correctly" in {
      test(new Timer(config1)) { implicit c =>
        initClocks
        var time = 10
        (1 to 50).foreach(_ => {
          logicalTime(time)
          (1 to config1.numAntiDependencies).foreach(_ => outPort)
          time += config1.interval
        })
        time = 4
        logicalTime(time)
        (1 to config1.numAntiDependencies).foreach(_ => outPort)
      }
    }
}