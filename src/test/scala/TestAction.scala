package reactor

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import org.scalatest._

class TestAction extends FlatSpec with ChiselScalatestTester with Matchers {

  def initClocks(c: Action[_ <: Data]): Unit = {
   c.io.nextEvent.initSink().setSinkClock(c.clock)
   c.io.outPort.initSink().setSinkClock(c.clock)
   c.io.schedule.initSource().setSourceClock(c.clock)
   c.io.logicalTime.initSource().setSourceClock(c.clock)
  }

  def sched(c: Action[_ <: Data], e: (Int,Int)): Unit = {
    c.io.schedule.enqueue(chiselTypeOf(c.io.schedule).bits.Lit(
      _.tag.tag -> e._1.U,
      _.value -> e._2.U
    ))
  }

  def logicalTime(c: Action[_ <: Data], e: Int): Unit = {
    c.io.logicalTime.enqueue(chiselTypeOf(c.io.logicalTime).bits.Lit(
      _.tag -> e.U
    ))
  }

  def nextEvent(c: Action[_ <: Data], e: Int): Unit = {
    c.io.nextEvent.expectDequeue(chiselTypeOf(c.io.nextEvent).bits.Lit(
      _.tag -> e.U
    ))
  }
  def outPort(c: Action[_ <: Data], e: (Int, Int)): Unit = {
    c.io.outPort.expectDequeue(chiselTypeOf(c.io.outPort).bits.Lit(
      _.tag.tag -> e._1.U,
      _.value -> e._2.U
    ))
  }

  implicit val rc = ReactorGlobalParams(numClkBits = 8)

  behavior of "Action"

  val config1 = ActionConfig(
    id = "test",
    gen = UInt(8.W),
    out = ActionPortConfig(id="test_2", UInt(8.W)),
    in = SchedulePortConfig(id="test_2_2", UInt(8.W)),
    numAntiDependencies = 3
  )

  it should "Initialize correctly" in {
    test(new Action(config1)) { c =>
      initClocks(c)
      c.io.busy.expect(false.B)
      c.io.logicalTime.ready.expect(true.B)
      c.io.outPort.valid.expect(false.B)
      c.io.nextEvent.valid.expect(false.B)
      c.io.schedule.ready.expect(true.B)
    }
  }

  it should "schedule event" in {
    test(new Action(config1)) { c =>
      initClocks(c)
      val e = (10,5)
      sched(c, e)
      nextEvent(c, e._1)
      for (i <- 0 until 100) {
        c.io.outPort.valid.expect(false.B)
        c.io.schedule.ready.expect(true.B)
      }
    }
  }
  it should "Schedule and advance logical clock" in {
    test(new Action(config1)) { c =>
      initClocks(c)
      val e = (10,5)
      sched(c, e)
      nextEvent(c, e._1)
      logicalTime(c, 5)
      c.io.outPort.valid.expect(false.B)
      logicalTime(c,10)
      outPort(c,e)
    }
  }

  it should "Schedule multiple and advance" in {
    test(new Action(config1)) { c =>
      initClocks(c)
      val e = Seq((10,5), (12,6), (5,10), (2,9), (1,9),(7,8))
      val s = e.sortWith((l,r) => l._1 < r._1)

      for (i <- 0 until e.length) {
        sched(c,e(i))
        nextEvent(c, e.take(i+1).sortWith((l,r) => l._1 < r._1)(0)._1)
      }
      s.foreach(e => {

        logicalTime(c,e._1)
        for (i <- 0 until config1.numAntiDependencies) {
          outPort(c,e)
        }
      })
    }
  }
}