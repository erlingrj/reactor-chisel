package reactor

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util.Valid


class TestModule extends Module {

  implicit val rc = ReactorGlobalParams(numClkBits = 8)
  val tag = RegInit(TimeTag(1.U(3.W)))


}


class TestTimeTag extends FlatSpec with ChiselScalatestTester with Matchers {


  behavior of "TestTag"

  it should "work" in {
    test(new TestModule()) {c =>

    }


  }

}
