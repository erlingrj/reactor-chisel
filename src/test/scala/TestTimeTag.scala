package reactor

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util.Valid
import org.scalatest.flatspec.AnyFlatSpec


class TestModule extends Module {

  implicit val rc = ReactorGlobalParams(numClkBits = 8)
  val tag = RegInit(TimeTag(1.U(3.W)))


}


class TestTimeTag extends AnyFlatSpec with ChiselScalatestTester {


  behavior of "TestTag"

  it should "work" in {
    test(new TestModule()) {c =>

    }


  }

}
