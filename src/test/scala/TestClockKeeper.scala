package reactor

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestClockKeeper extends AnyFlatSpec with ChiselScalatestTester {

  def initClocks(implicit c: ClockKeeper): Unit = {
    c.io.nextEvent.initSink().setSinkClock(c.clock)
    c.io.logicalTimeIn.initSource().setSourceClock(c.clock)
    c.io.logicalTimeOut.initSink().setSinkClock(c.clock)
    c.io.getTags().map(_.initSource().setSourceClock(c.clock))
    c.io.waitForFinish.initSource().setSourceClock(c.clock)
  }


  def logicalTimeIn(t: Int)(implicit c: ClockKeeper): Unit = {
    c.io.logicalTimeIn.enqueue(chiselTypeOf(c.io.logicalTimeIn).bits.Lit(
      _.tag -> t.U
    ))
  }

  def logicalTimeOut(t: Int)(implicit c: ClockKeeper): Unit = {
    c.io.logicalTimeOut.expectDequeue(chiselTypeOf(c.io.logicalTimeIn).bits.Lit(
      _.tag -> t.U
    ))
  }

  def logicalTime(t: Int)(implicit  c: ClockKeeper): Unit = {
    logicalTimeIn(t)
    logicalTimeOut(t)
  }

  def nextEvent(t: Int)(implicit c: ClockKeeper): Unit = {
    c.io.nextEvent.expectDequeue(chiselTypeOf(c.io.nextEvent).bits.Lit(
      _.tag -> t.U
    ))
  }

  def setTimerTag(i: Int, t: Int)(implicit c: ClockKeeper): Unit = {
    c.io.timerTags(i).valid.poke(true.B)
    c.io.timerTags(i).bits.tag.poke(t.U)
  }
  def setReactorTag(i: Int, t: Int)(implicit c: ClockKeeper): Unit = {
    c.io.reactorTags(i).valid.poke(true.B)
    c.io.reactorTags(i).bits.tag.poke(t.U)
  }
  def setActionTag(i: Int, t: Int)(implicit c: ClockKeeper): Unit = {
    c.io.actionTags(i).valid.poke(true.B)
    c.io.actionTags(i).bits.tag.poke(t.U)
  }
  def unsetActionTag(i: Int)(implicit c: ClockKeeper): Unit = {
    c.io.actionTags(i).valid.poke(false.B)
  }
  def unsetTimerTag(i: Int)(implicit c: ClockKeeper): Unit = {
    c.io.timerTags(i).valid.poke(false.B)
  }
  def unsetReactorTag(i: Int)(implicit c: ClockKeeper): Unit = {
    c.io.reactorTags(i).valid.poke(false.B)
  }

  def waitForFinish(implicit c: ClockKeeper): Unit = {
    c.io.waitForFinish.enqueue(chiselTypeOf(c.io.waitForFinish.bits))
    c.clock.step(1)
  }

  implicit val rc = ReactorGlobalParams(numClkBits = 8)

  behavior of "ClockKeeper"

  implicit var config1 = ExampleConfig.topReactor

  it should "Initialize correctly" in {
    test(new ClockKeeper(config1)) { implicit c =>
      initClocks
      c.io.logicalTimeOut.valid.expect(false.B)
      c.io.logicalTimeIn.ready.expect(true.B)
      c.io.nextEvent.valid.expect(false.B)
      c.io.waitForFinish.ready.expect(false.B)
    }
  }
  it should "Accept and redistribute logical time" in {
    test(new ClockKeeper(config1)) { implicit c =>
      initClocks
      logicalTimeIn(42)
      logicalTimeOut(42)
    }
  }
  it should "Accept tag" in {
    test(new ClockKeeper(config1)) { implicit c =>
      initClocks
      logicalTime(42)
      setActionTag(0, 45)
      setTimerTag(0, 43)
      waitForFinish
      nextEvent(43)
    }
  }
  it should "Accept multiple tag" in {
    test(new ClockKeeper(config1)) { implicit c =>
      initClocks

      (1 to 10) foreach (i => {
        logicalTime(i*10)
        setActionTag(0, i*10 + 15)
        setTimerTag(0, (i+1)*10)
        waitForFinish
        nextEvent((i+1)*10)
      })
    }
  }

  it should "Accept time without new tags" in {
    test(new ClockKeeper(config1)) { implicit c =>
      initClocks
      logicalTime(42)
      setActionTag(0, 100)
      setTimerTag(0, 110)
      waitForFinish
      nextEvent(100)
      logicalTime(99)
      waitForFinish
      nextEvent(100)
      logicalTime(100)
      unsetActionTag(0)
      waitForFinish
      nextEvent(110)
    }
  }
  it should "Wait while action is busy" in {
    test(new ClockKeeper(config1)) { implicit c =>
      initClocks
      logicalTime(42)
      c.io.actionsBusy(0).poke(true.B)
      c.clock.step(100)
      c.io.actionsBusy(0).poke(false.B)
      setActionTag(0, 59)
      waitForFinish
      nextEvent(59)
    }
  }
}

