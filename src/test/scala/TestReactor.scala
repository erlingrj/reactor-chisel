package reactor.test
import reactor._
import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec



class Reactor1 extends Reactor {
  val gen = new SingleToken(UInt(8.W))
  class Reactor1IO extends ReactorIO {
    val in = Vec(1, new EventReadMaster(gen))
    val out = new EventWriteMaster(gen)
  }
  val io = IO(new Reactor1IO)
  val r1 = Module(new ReactionAddN(1))
  val r2 = Module(new ReactionAddN(2))

  val in = Module(new InputPort(InputPortConfig(gen, 2)))
  val out = Module(new OutputPort(OutputPortConfig(gen, 2)))

  in.connectDownstream(r1.io.in)
  in.connectDownstream(r2.io.in)
  in.connectUpstream(io.in(0))

  out.connectUpstream(r1.io.out)
  out.connectUpstream(r2.io.out)
  out.connectDownstream(io.out)

  override val reactions = Seq(r1, r2)
  override val inPorts = Seq(in)
  override val outPorts = Seq(out)
}
class TestReactor extends AnyFlatSpec with ChiselScalatestTester {


  behavior of "ExampleReactor"
  it should "initialize" in {
    test(new Reactor1) {c =>
      implicit val clk = c.clock
    }
  }
  it should "Respect precedence" in {
    test(new Reactor1) { c =>
      implicit val clk = c.clock
    }
  }
}
