package reactor.test

import reactor._

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.VecLiterals._


object ReactionSim {
  def trigger[T <: Data](c: EventReadMaster[SingleToken[T]], data: T)(implicit clk: Clock): Unit = {
    timescope {
      c.resp.valid.poke(true.B)
      c.resp.present.poke(true.B)
      c.resp.token.data.poke(data)
      while (!c.fire.peekBoolean()) {
        clk.step()
      }

    }
  }


  def expOutput[T <: Data](c: EventWriteMaster[SingleToken[T]], data: T)(implicit clk: Clock): Unit = {
    timescope {
      while (!c.req.valid.peekBoolean()) {
        clk.step()
      }
      c.req.valid.expect(true.B)
      c.req.present.expect(true.B)
      c.req.token.data.expect(data)
      clk.step()
    }
  }

  def expOutputAndFire[T <: Data](c: EventWriteMaster[SingleToken[T]], data: T)(implicit clk: Clock): Unit = {
    timescope {
      while (!c.req.valid.peekBoolean()) {
        clk.step()
      }
      c.req.valid.expect(true.B)
      c.req.present.expect(true.B)
      c.req.token.data.expect(data)
      clk.step()
      while (!c.fire.peekBoolean()) {
        clk.step()
      }
    }
  }
}

class R1IO extends ReactionIO {
  val in = new EventReadMaster(gen = new SingleToken(UInt(8.W)))
  val out = new EventWriteMaster(gen = new SingleToken(UInt(8.W)))
}
class ReactionAddN(n: Int, c: ReactionConfig = ReactionConfig(0,0)) extends Reaction {
  val io = IO(new R1IO)
  override val triggers = Seq(io.in)
  override val antiDependencies = Seq(io.out)

  // Bring the port into scope
  val in = io.in
  val out = io.out

  def reactionBody = {
    val read = in.resp.token.data
    val filtered = read + n.U
    out.req.write(filtered)
    reactionDone := true.B
  }

  reactionMain()
}




class TestReaction extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "R1"
  it should "initialize" in {
    test(new ReactionAddN(1)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val clk = c.clock
      clk.step(5)
      c.in.fire.expect(false.B)
      c.out.req.valid.expect(false.B)
    }
  }

  it should "fire and consume" in {
    test(new ReactionAddN(1)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val clk = c.clock
      clk.step(5)
      fork {
        ReactionSim.trigger(c.io.in, 13.U)
      }.fork.withRegion(Monitor) {
        ReactionSim.expOutput(c.io.out, 14.U)
      }.joinAndStep(clk)
    }
  }
}
