package reactor

import chisel3._
import chisel3.util._

/**
 *
 * Arbiter controls the enable signals to all the reactions.
 *  It assumes that the reactions are connected in precedence order
 *  The reactions are enabled one after the other. The Reaction can start executing as soon as the
 *  enable signal is high. And when it is done executing it asserts the "ready" signal
 *  At which point the Arbiter moves on to the next Reaction
 */


class ReactionArbiterIO(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Bundle {

 // Enable-interface to scheduler so that things can be stalled until we are at a consistent logical time
 val enableIn = Flipped(Decoupled())

 val enableOut = Vec(c.numReactions, Flipped(new ReactionEnableIO))

 def tieOff: Unit = {
  enableIn.ready := false.B
  enableOut.map(_.enable := false.B)
 }
}

class ReactionArbiter(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Module {
 val io = IO(new ReactionArbiterIO(c))
 io.tieOff

 val sIdle :: sArbiting :: sBlock :: Nil = Enum(3)
 val regState = RegInit(sIdle)
 val regCount = RegInit(0.U(log2Ceil(c.numReactions).W))


 switch(regState) {
  is(sIdle) {
   io.enableIn.ready := true.B
   when(io.enableIn.fire) {
    regState := sArbiting
   }
  }
  is(sArbiting) {
   io.enableOut(regCount).enable := true.B
   when(io.enableOut(regCount).fire) {
    regState := sBlock
   }.otherwise {
    regCount := regCount + 1.U
    when(regCount === (c.numReactions - 1).U) {
     regState := sIdle
     regCount := 0.U
    }
   }
  }

  is(sBlock) {
   io.enableOut(regCount).enable := true.B
   when(io.enableOut(regCount).done) {
    regCount := regCount + 1.U
    when(regCount === (c.numReactions - 1).U) {
     regState := sIdle
     regCount := 0.U
    }
   }
  }
 }
}

