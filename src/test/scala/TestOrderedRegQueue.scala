package reactor

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import org.scalatest._

class TestOrderedRegQueue extends FlatSpec with ChiselScalatestTester with Matchers {

  def initClocks(c: EventQueue[_ <: Data]): Unit = {
    c.io.enq.initSource().setSourceClock(c.clock)
    c.io.deq.initSink().setSinkClock(c.clock)
  }

  def enq(c: EventQueue[_<:Data], event: (Int, Int)) = {
    c.io.enq.enqueue(chiselTypeOf(c.io.enq).bits.Lit(
      _.value -> event._2.U,
      _.tag.tag -> event._1.U
    ))
  }

  def deq(c: EventQueue[_<:Data], event: (Int, Int)) = {
    c.io.deq.expectDequeue(chiselTypeOf(c.io.deq).bits.Lit(
      _.value -> event._2.U,
      _.tag.tag -> event._1.U
    ))
  }


  implicit val rc = ReactorGlobalParams(numClkBits = 8)
  behavior of "OrderedRegQueue"

  val config1 = EventQueueConfig(
    size = 8,
    gen = UInt(8.W)
  )

  it should "Initialize correctly" in {
    test(new OrderedRegQueue(config1)) { c =>
      initClocks(c)
      c.io.enq.ready.expect(true.B)
      c.io.deq.valid.expect(false.B)
      c.io.size.expect(0.U)
      c.io.sorting.expect(false.B)
    }
  }

  it should "simple enq-deq" in {
    test(new OrderedRegQueue(config1)) { c =>
      initClocks(c)
      val e = (1,5)
      enq(c, e)
      deq(c, e)
    }
  }

  it should "sort single" in {
    test(new OrderedRegQueue(config1)) { c =>
      initClocks(c)
      val e = Seq((4,2),(3,1))
      enq(c, e(0))
      enq(c, e(1))
      deq(c, e(1))
      deq(c, e(0))
    }
  }

  it should "sort many" in {
    test(new OrderedRegQueue(config1)) { c =>
      initClocks(c)
      val e = Seq((4,2),(3,1),(10,4), (1,3),(2,5),(7,9))
      val sorted = e.sortWith((l,r) => l._1 < r._1)
      e.foreach(e => enq(c,e))
      sorted.foreach(s => deq(c,s))
    }
  }
  it should "fill uo" in {
    test(new OrderedRegQueue(config1)) { c =>
      initClocks(c)
      val e = Seq((5,2),(2,1),(3,4), (1,3),(9,5),(7,9), (8,2), (0,3))
      val sorted = e.sortWith((l,r) => l._1 < r._1)
      e.foreach(e => enq(c,e))
      for (i <- 0 until 30) {
        c.io.enq.ready.expect(false.B)
        c.clock.step()
      }

      deq(c, sorted(0))
      enq(c, (10,4))

      sorted.drop(1).foreach(s => deq(c,s))
      deq(c, (10,4))



    }
  }

}
