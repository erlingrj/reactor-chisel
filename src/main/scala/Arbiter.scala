package reactor

import chisel3._
import chisel3.util._

/**
 *
 * TODO: Should be SINGLE arbiter per Reactor and deal with both Ports and Actions.
 *  this is because the presedence could be mixed among reactions triggered by ports and actions
 *  This will  complicate the Arbiter design
 */


class ArbiterIO(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Bundle {

 // Enable-interface to scheduler so that things can be stalled until we are at a consistent logical time
 val enableIn = Flipped(Decoupled((Bool())))

 val enableOut = Vec(c.numReactions, Flipped(new ReactionEnableIO))

 def tieOff: Unit = {
  enableIn.ready := false.B
  enableOut.map(_.enable := false.B)
 }
}

class Arbiter(c: ReactorConfig)(implicit rc: ReactorGlobalParams) extends Module {
 val io = IO(new ArbiterIO(c))
 io.tieOff

 val sIdle :: sArbiting :: sBlock :: Nil = Enum(3)
 val regState = RegInit(sIdle)
 val regCount = RegInit(0.U(log2Ceil(c.numReactions).W))


 switch(regState) {
  is (sIdle) {
   io.enableIn.ready := true.B
   when (io.enableIn.fire) {
    regState := sArbiting
   }
  }
 }

 is (sArbiting) {
  io.enableOut(regCount).enable := true.B
  when(io.enableOut(regCount).fire) {
   regState := sBlock
   }.otherwise {
    regCount := regCount + 1.U
    when (regCount === (c.numReactions - 1).U) {
     regState := sIdle
     regCount := 0.U
    }
  }
 }

 is (sBlock) {
  io.enableOut(regCount).enable := true.B
  when (io.enableOut(regCount).done) {
   regCount := regCount + 1.U
   when (regCount === (c.numReactions - 1).U) {
    regState := sIdle
    regCount := 0.U
   }
  }
 }
}
