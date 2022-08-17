package reactor

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._


class VaddReaction(len: Int)(implicit val rc: ReactorGlobalParams) extends Reaction {

  val portIOConfig = PortIOConfig(nElems = len)

  val out1 = IO(Flipped(new PortInIO(portIOConfig, UInt(8.W))))
  val in1 = IO(Flipped(new PortOutIO(portIOConfig, UInt(8.W))))
  val in2 = IO(Flipped(new PortOutIO(portIOConfig, UInt(8.W))))

  override val triggers = Seq(in1, in2)
  override val antiDependencies = Seq(out1)
  override val dependencies = Seq()

  val regIdx = RegInit(0.U(log2Ceil(len).W))
  val regRead = RegInit(0.U(8.W))

  def reactionBody: Unit = {
    val done = WireInit(false.B)

    val s1_regIdx = RegInit(0.U(8.W))

    val s2_valid = RegInit(false.B)
    val s2_in1 = RegInit(0.U(8.W))
    val s2_in2 = RegInit(0.U(8.W))
    val s2_regIdx = RegInit(0.U(8.W))

    val s3_valid = RegInit(false.B)
    val s3_res = RegInit(0.U(8.W))
    val s3_regIdx = RegInit(0.U(8.W))

    // S1 Read data
    in1.addr := s1_regIdx
    in1.en := true.B
    in2.addr := s1_regIdx
    in2.en := true.B
    s2_valid := true.B
    s2_regIdx := s1_regIdx
    s2_in1 := in1.data
    s2_in2 := in2.data

    // S2: Data receive data
    s3_valid := s2_valid
    s3_res := s2_in1 + s2_in2
    s3_regIdx := s2_regIdx

    // S3 : Write data
    when (s3_valid) {
      out1.en := true.B
      out1.addr := s3_regIdx
      out1.data := s3_res

      // Check for finished
      when(s3_regIdx === (len - 1).U) {
        done := true.B
      }
    }

    when (!done) {

      s1_regIdx := s1_regIdx + 1.U
    } otherwise{
      s1_regIdx := 0.U
    }

    reactionDone := done
  }

  reactionPrelude
  reactionMain
}

class TestReaction extends FlatSpec with ChiselScalatestTester with Matchers {

  def initClocks(c: Reaction): Unit = {
    c.ioCtrl.enable.initSource().setSourceClock(c.clock)
  }

  def expRead(c: Reaction, idx: Int, eAddr: Int, value: Int): Unit = {
    val port = c.triggers(idx)
    while (!port.en.peekBoolean()) {
      c.clock.step()
    }
    port.addr.expect(eAddr.U)
    port.data.poke(value.U)
    c.clock.step()
  }

  def expWrite(c: Reaction, idx: Int, eAddr: Int, eValue: Int): Unit =  {
    val port = c.antiDependencies(idx)
    while (!port.en.peekBoolean()) {
      c.clock.step()
    }
    port.addr.expect(eAddr.U)
    port.data.expect(eValue.U)
    c.clock.step()
  }

  val rc = ReactorGlobalParams()
  behavior of "VaddReaction"

  it should "initialize" in {
    test(new VaddReaction(5)(rc)) {c =>
      initClocks(c)
      c.out1.en.expect(false.B)
      c.in1.data.expect(0.U)
      c.in2.data.expect(0.U)
    }
  }

  it should "Compute one correctly" in {
    test(new VaddReaction(10)(rc)) { c =>
      initClocks(c)
      c.ioCtrl.enable.enqueueNow(chiselTypeOf(c.ioCtrl.enable.bits))
      c.ioCtrl.running.expect(true.B)
      fork {
        expRead(c,0,0,5)
      }.fork {
        expRead(c,1,0,10)
      }.fork.withRegion(Monitor) {
        expWrite(c, 0, 0, 15)
      }.joinAndStep(c.clock)
    }
  }

  it should "Compute all correctly" in {
    test(new VaddReaction(3)(rc)) { c =>
      initClocks(c)
      val i = 5
      c.ioCtrl.enable.enqueueNow(chiselTypeOf(c.ioCtrl.enable.bits))
      c.ioCtrl.running.expect(true.B)
      fork {
        expRead(c, 0, 0, 5)
        expRead(c, 0, 1, 10)
        expRead(c, 0, 2, 15)
      }.fork {
        expRead(c, 1, 0, 15)
        expRead(c, 1, 1, 10)
        expRead(c, 1, 2, 5)
      }.fork.withRegion(Monitor) {
        expWrite(c, 0, 0, 20)
        expWrite(c, 0, 1, 20)
        expWrite(c, 0, 2, 20)
      }.joinAndStep(c.clock)
    }
  }


  it should "Do end sync correctly" in {
    test(new VaddReaction(3)(rc)) { c =>
      initClocks(c)
      c.ioCtrl.enable.enqueueNow(chiselTypeOf(c.ioCtrl.enable.bits))
      c.ioCtrl.running.expect(true.B)
      while(!c.ioCtrl.done.peekBoolean()) {
        c.ioCtrl.running.expect(true.B)
        c.clock.step()
      }
      c.ioCtrl.running.expect(true.B)
      c.clock.step()
      c.ioCtrl.running.expect(false.B)
    }
  }


  it should "Support multiple firings" in {
    test(new VaddReaction(3)(rc)) { c =>
      val in1 = Seq(
        Seq(1,2,3),
        Seq(4,5,6),
        Seq(7,8,9)
      )
      val in2 = Seq(
        Seq(22,31,2),
        Seq(5,43,1),
        Seq(32,21,3)
      )

      initClocks(c)
      for (i <- 0 until 3) {
        c.ioCtrl.enable.enqueueNow(chiselTypeOf(c.ioCtrl.enable.bits))
        c.ioCtrl.running.expect(true.B)
        fork {
          var idx = 0
          for (v <- in1(i)) {
            expRead(c, 0, idx, v)
            idx += 1
          }
        }.fork {
          var idx = 0
          for (v <- in2(i)) {
            expRead(c, 1, idx, v)
            idx += 1
          }
        }.fork.withRegion(Monitor) {
          var idx = 0
          for ((v1,v2) <- in1(i) zip in2(i)) {
            expWrite(c, 0, idx, v1+v2)
            idx += 1
          }
        }.joinAndStep(c.clock)
      }
      }
    }

  }
