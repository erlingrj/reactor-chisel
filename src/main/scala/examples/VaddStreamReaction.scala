package reactor.examples

import chisel3._
import reactor._
import reactor.lib.StreamJoin



class VaddStreamReaction(c: ReactionConfig) extends ReactionStreaming(c) {

  def reactionBody: Unit = {
    val done = WireInit(false.B)

    def add(in1: UInt, in2: UInt): UInt = {
      in1+in2
    }

    val streamAdder = Module(new StreamJoin(UInt(8.W), UInt(8.W), UInt(8.W), add)).io
    streamAdder.in1 <> triggerReader(0).out
    streamAdder.in2 <> triggerReader(1).out
    streamAdder.out <> antiDependencyWriter(0).in

    when(antiDependencyWriter(0).done) {
      done := true.B
    }
    reactionDone := done
  }
  reactionMain
}

