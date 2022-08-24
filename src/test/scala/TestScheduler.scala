package reactor

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec



class TestScheduler extends AnyFlatSpec with ChiselScalatestTester  {

  def initClocks(c: Scheduler): Unit = {
    c.ioSchedulerCtrl.start.initSource().setSourceClock(c.clock)
    c.ioReactionCtrl.map(_.enable.initSink().setSinkClock(c.clock))
  }

  def expReactionEnable(c: Scheduler, idx: Int, start: Int = -1) = {
      val r = c.ioReactionCtrl(idx)
      if (start == -1) {
        while (!r.enable.valid.peekBoolean()) {
          c.clock.step()
        }
      }
      r.enable.valid.expect(true.B)
      r.enable.ready.poke(true.B)
      c.clock.step()
    }

  def reactionDone(c: Scheduler, idx: Int) = {
      c.ioReactionCtrl(idx).done.poke(true.B)
      c.clock.step()
  }

  def simReaction(c: Scheduler, idx: Int, start: Int, duration: Int) = {
    timescope {
      expReactionEnable(c,idx,start)
      c.ioReactionCtrl(idx).running.poke(true.B)
      c.clock.step(duration)
      reactionDone(c,idx)
    }
  }

  def scheduleDone(c: Scheduler): Unit = {
    c.clock.step(2)
    c.ioSchedulerCtrl.done.expect(true.B)
    c.ioSchedulerCtrl.running.expect(true.B)
    c.clock.step()
    c.ioSchedulerCtrl.running.expect(false.B)
  }

  def scheduleStart(c: Scheduler): Unit = {
    c.ioSchedulerCtrl.start.enqueueNow(chiselTypeOf(c.ioSchedulerCtrl.start.bits))
  }
  val rc = ReactorGlobalParams()
  val c1 = SchedulerConfig(
    schedule = Seq(Seq(0))
  )
  val c2 = SchedulerConfig(
    schedule = Seq(
      Seq(1, 3),
      Seq(0, 2),
      Seq(4)
    )
  )


  behavior of "Scheduler"

  it should "initialize" in {
    test(new Scheduler(c1)(rc)) { c =>
      initClocks(c)
      c.ioSchedulerCtrl.start.ready.expect(true.B)
      c.ioReactionCtrl.map(_.enable.valid.expect(false.B))
    }
  }

  it should "enable reaction" in {
    test(new Scheduler(c1)(rc)) { c =>
      initClocks(c)
      c.ioSchedulerCtrl.start.enqueueNow(chiselTypeOf(c.ioSchedulerCtrl.start.bits))
      simReaction(c,0,-1,5)
      scheduleDone(c)
    }
  }

  it should "support multiple firings" in
  test(new Scheduler(c1)(rc)) { c =>
    initClocks(c)
    val durations = Seq(5,10,14,23,4)
    for (i <- 0 until 5) {
      c.ioSchedulerCtrl.start.enqueueNow(chiselTypeOf(c.ioSchedulerCtrl.start.bits))
      simReaction(c,0,-1,durations(i))
      scheduleDone(c)
    }
  }

  it should "support more complex schedule" in {
    test(new Scheduler(c2)(rc)) { c =>
      initClocks(c)
      c.ioSchedulerCtrl.start.ready.expect(true.B)
      c.ioReactionCtrl.map(_.enable.valid.expect(false.B))

      scheduleStart(c)
      fork {
        simReaction(c,1,-1,10)
      }.fork {
        simReaction(c,3,-1,16)
      }.join()

      fork {
        simReaction(c,0,-1,8)
      }.fork{
        simReaction(c,2,-1,12)
      }.join()

      simReaction(c,4,-1,13)
      scheduleDone(c)
    }
  }
}
