import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import reactor._
import reactor.globals._
import reactor.test.ReactorSim

class TestState extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SingleValue State"
  it should "initialize" in {
    test(new SingleValueState(StateConfig(defData, defToken, nReactions = 1, protocol = Immediate))) { c =>
      assert(c.io.ports.length == 1)
      val read = c.io.ports(0).read
      val write = c.io.ports(0).write

      read.resp.data.expect(0.U)

      c.clock.step(1)
      write.req.data.poke(12.U)
      c.clock.step()
      read.resp.data.expect(0.U)
      write.req.write.poke(true.B)
      c.clock.step()
      read.resp.data.expect(12.U)
    }
  }
}
