package reactor.test

import chisel3._
import chisel3.util._
import chiseltest._
import reactor._

// FIXME: Generalize to other types of Tokens? Maybe we should capture token in a single paramter
object ReactorSim {
  def readSlave[T <: Data](c: EventReadMaster[T, SingleToken[T]], data: T, expFire: Boolean=true, present: Boolean = true)(implicit clk: Clock): Unit = {
    // FIXME: Should also accept Vec of EventReadMaster, if driving top-level input of Reactor, then we wanna drive all inputs together, maybe
    require(expFire)

    timescope {
      c.resp.valid.poke(true.B)
      c.resp.present.poke(present.B)
      c.resp.token.data.poke(data)
      if (expFire) {
        while (!c.fire.peekBoolean()) {
          clk.step()
        }
        c.resp.valid.poke(false.B)
        c.resp.present.poke(false.B)
      }
      clk.step()
    }
  }

  def readMaster[T <: Data](c: EventReadSlave[T, SingleToken[T]], expData: T, fire: Boolean=true, present: Boolean = true, now: Boolean = false)(implicit clk: Clock): Unit = {
    timescope {
      if (!now) {
        while (!c.resp.valid.peekBoolean()) {
          clk.step()
        }
      }
      c.resp.valid.expect(true.B)
      c.resp.present.expect(present.B)
      c.resp.token.data.expect(expData)
      c.fire.poke(fire.B)
      clk.step()
    }
  }

  def writeMaster[T <: Data](c: EventWriteSlave[T, SingleToken[T]], data: T, present: Boolean = true, fire: Boolean=true)(implicit clk: Clock): Unit = {
    timescope {
      c.req.valid.poke(true.B)
      c.req.present.poke(true.B)
      c.req.token.data.poke(data)
      if (fire) {
        c.fire.poke(true.B)
      }
      clk.step()
    }
  }

  // FIXME: We also want to receive a bunch of writes and only expect on the last one (essentially emulate a connection)
  def writeSlave[T <: Data](c: EventWriteMaster[T, SingleToken[T]], expData: T, present: Boolean = true, fire: Boolean = true, now: Boolean = false)(implicit clk: Clock): Unit = {
    timescope {
      if (!now) {
        while (!c.req.valid.peekBoolean()) {
          clk.step()
        }
      }

      c.req.valid.expect(true.B)
      c.req.present.expect(present.B)
      c.req.token.data.expect(expData)

      if (fire) {
        while (!c.fire.peekBoolean()) {clk.step()}
        c.fire.expect(true.B)
      }

      clk.step()
    }
  }
}
