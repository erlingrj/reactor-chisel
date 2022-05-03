package reactor

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util.Valid


class TestModule extends Module {

  val rc = ReactorGlobalParams(numClkBits = 8)
  val tag = TimeTag(0.U(8.W))(rc)


}


class TestTimeTag extends FlatSpec with ChiselScalatestTester with Matchers {


  behavior of "TestTag"

  it should "work" in {
    test(new TestModule()) {c =>

    }


  }

}
