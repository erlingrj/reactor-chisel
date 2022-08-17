package reactor.lib

import chisel3._
import chisel3.util._

import reactor._


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
