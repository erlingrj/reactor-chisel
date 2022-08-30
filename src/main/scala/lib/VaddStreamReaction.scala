package reactor.lib

import chisel3._
import chisel3.util._

import reactor._



class VaddStreamReaction(len: Int) extends Reaction {

  val gen = UInt(8.W)

  val portIOConfig = PortIOConfig(nElems = len, gen=gen)

  val out = IO(Flipped(new PortInIO(portIOConfig)))
  val in = IO(Flipped(new PortOutIO(portIOConfig)))


  override val triggers = Seq(in)
  override val antiDependencies = Seq(out)
  override val dependencies = Seq()

  def reactionBody: Unit = {
    val done = WireInit(false.B)

    // TODO: Should this be a trait hasStreamingPortReader hasStreamingPortWriter
    val readerConfig = PortIOConfig(nElems = len, gen = gen)
    val reader = Module(new PortStreamReader(readerConfig))
    reader.io.tieOffExt()
    in <> reader.io.portRead

    val writerConfig = PortIOConfig(nElems = len/2, gen=gen)
    val writer = Module(new PortStreamWriter(writerConfig))
    writer.io.tieOffExt()
    out <> writer.io.portWrite

    def add2(vec: Vec[UInt]): UInt = {
      vec(0) + vec(1)
    }

    val streamAdder = Module(new StreamMapWithStride(gen, 2, add2))
    streamAdder.io.in <> reader.io.out
    streamAdder.io.out <> writer.io.in

    val sIdle :: sRunning :: sDone :: Nil = Enum(3)
    val regState = RegInit(sIdle)

    switch(regState) {
      is(sIdle) {
        reader.io.start.valid := true.B
        when(reader.io.start.fire) {
          regState := sRunning
        }
      }
      is(sRunning) {
        when(writer.io.done) {
          regState := sDone
        }
      }
      is(sDone) {
        done := true.B
      }
    }
    reactionDone := done
  }
  reactionPrelude
  reactionMain
}

