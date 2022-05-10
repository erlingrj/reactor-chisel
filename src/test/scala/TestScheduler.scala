package reactor

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestScheduler extends AnyFlatSpec with ChiselScalatestTester {

  def initClocks(implicit c: Scheduler): Unit = {
    c.io.arbiterEn.initSink().setSinkClock(c.clock)
    c.io.clockKeeperStart.initSink().setSinkClock(c.clock)
    c.io.execute.initSource().setSourceClock(c.clock)
    c.io.reactorsEn.map(_.initSink().setSinkClock(c.clock))
  }

  def execute(implicit c: Scheduler): Unit = {
    c.io.execute.enqueue(chiselTypeOf(c.io.execute.bits))
  }

  def arbiterEn(t: Int)(implicit c: Scheduler): Unit = {
    val x = c.io.arbiterEn
    timescope {
      x.ready.poke(true.B)
      fork.withRegion(Monitor) {
        x.waitForValid()
        x.valid.expect(true.B)
        c.clock.step(1)
        x.ready.poke(false.B)
        c.clock.step(t)
        x.ready.poke(true.B)
      }
    }
  }

  def reactorEn(i: Int, t: Int)(implicit c: Scheduler): Unit = {
    val x = c.io.reactorsEn(i)
    timescope {
      x.ready.poke(true.B)
      fork.withRegion(Monitor) {
        x.waitForValid()
        x.valid.expect(true.B)
        c.clock.step(1)
        x.ready.poke(false.B)
        c.clock.step(t)
        x.ready.poke(true.B)
      }.joinAndStep(c.clock)
    }
  }

  def clockKeeperStart(implicit c: Scheduler): Unit = {
    c.io.clockKeeperStart.expectDequeue(chiselTypeOf(c.io.clockKeeperStart.bits))
  }

  implicit val rc = ReactorGlobalParams(numClkBits = 8)

  behavior of "Scheduler"

  // Simple config with 2 sequential contained reactors and 2 actions and 2 reactions
  implicit val config1 = ReactorConfig(
    id = "TestReactor",
    actions = Seq(ActionConfig(), ActionConfig()),
    reactions = Seq(ReactionConfig(), ReactionConfig()),
    reactors = Seq(ReactorConfig(), ReactorConfig()),
    reactorsLevel = Seq(0, 1)
  )

  it should "Initialize correctly" in {
    test(new Scheduler(config1)) { implicit c =>
      initClocks
      c.io.execute.ready.expect(true.B)
      c.io.arbiterEn.valid.expect(false.B)
      c.io.reactorsEn.map(_.valid.expect(false.B))
      c.io.clockKeeperStart.valid.expect(false.B)
    }
  }

  it should "enable stuff in the right order" in {
    test(new Scheduler(config1)) { implicit c =>
      initClocks
      execute
      fork {
        reactorEn(0, 10)
        reactorEn(1, 10)
      }.fork {
        arbiterEn(10)
      }.join()
      clockKeeperStart
    }
  }

  it should "multiple rounds" in {
    test(new Scheduler(config1)) { implicit c =>
      initClocks
      execute
      fork {
        reactorEn(0, 5)
        reactorEn(1, 8)
      }.fork {
        arbiterEn(20)
      }.join()
      clockKeeperStart

      // round 2
      execute
      fork {
        reactorEn(0, 5)
        reactorEn(1, 8)
      }.fork {
        arbiterEn(20)
      }.join()
      clockKeeperStart

    }
  }


  implicit val config2= ReactorConfig(
    id = "TestReactor",
    actions = Seq(ActionConfig(), ActionConfig()),
    reactions = Seq(ReactionConfig(), ReactionConfig()),
  )

  it should "work without containing reactors" in {
    test(new Scheduler(config2)) { implicit c =>
      initClocks
      execute
      arbiterEn(15)
      clockKeeperStart
    }
  }
  it should "work without containing reactors in multiple rounds" in {
    test(new Scheduler(config2)) { implicit c =>
      initClocks
      (1 to 10).foreach(_ => {
        execute
        arbiterEn(15)
        clockKeeperStart
      })
    }
  }
}

