package reactor

import chisel3.ActualDirection.Empty
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}

class TestReactionArbiter extends FlatSpec with ChiselScalatestTester with Matchers {

  def initClocks(implicit c: ReactionArbiter): Unit = {
    c.io.enableIn.initSource().setSourceClock(c.clock)
  }

  def enableIn(implicit c: ReactionArbiter): Unit = {
    c.io.enableIn.valid.poke(true.B)
    fork
      .withRegion(Monitor) {
        while(!c.io.enableIn.ready.peekBoolean()) {
          c.clock.step()
        }
        c.io.enableIn.ready.expect(true.B)
      }
      .joinAndStep(c.clock)
  }

  def enableOut(idx: Int, execTime: Int = 1)(implicit c: ReactionArbiter): Unit = {
    fork
      .withRegion(Monitor) {
        while(!c.io.enableOut(idx).enable.peekBoolean()) {
          c.clock.step()
        }
        c.io.enableOut(idx).enable.expect(true.B)
        c.io.enableOut(idx).fire.poke(true.B)
        (1 to execTime)foreach(_ => c.clock.step())
        c.io.enableOut(idx).done.poke(true.B)
        c.clock.step()
      }.joinAndStep(c.clock)
  }

  implicit val rc = ReactorGlobalParams(numClkBits = 8)

  behavior of "ReactionArbiter"

  implicit val config = ExampleConfig.topReactor

  it should "Initialize correctly" in {
    test(new ReactionArbiter(config)) { implicit c =>
      initClocks
      c.io.enableIn.ready.expect(true.B)
      c.io.enableOut.map(_.enable.expect(false.B))
    }
  }

  it should "forward enabling" in {
    test(new ReactionArbiter(config)) { implicit c =>
      initClocks
      enableIn
      enableOut(0)
      enableOut(1)
    }
  }
  it should "forward enabling with long exec time" in {
    test(new ReactionArbiter(config)) { implicit c =>
      initClocks
      enableIn
      enableOut(0, 100)
      enableOut(1, 100)
    }
  }
  it should "multiple rounds" in {
    test(new ReactionArbiter(config)) { implicit c =>
      initClocks
      (0 to 10).foreach(_ => {
        enableIn
        enableOut(0)
        enableOut(1)
      })

    }
  }
}