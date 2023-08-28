package reactor.test

import chisel3._
import chisel3.util._
import chiseltest._
import reactor._

// FIXME: Generalize to other types of Tokens? Maybe we should capture token in a single paramter
object ReactorSim {
  val Monitor2 = Region

  def readSlave[T <: Data](c: TokenReadMaster[T, SingleToken[T]], data: T, expFire: Boolean=true, present: Boolean = true)(implicit clk: Clock): Unit = {
    // FIXME: Should also accept Vec of TokenReadMaster, if driving top-level input of Reactor, then we wanna drive all inputs together, maybe
//    require(expFire)

    timescope {
      c.resp.valid.poke(true.B)
      c.token.poke(true.B)
      c.present.poke(present.B)
      c.resp.bits.data.poke(data)

      clk.step()
      if (expFire) {
        fork.withRegion(Monitor) {
          while (!c.fire.peekBoolean()) {
            clk.step()
          }
        }.joinAndStep(clk)
      }
    }
  }

  def readMaster[T <: Data](c: TokenReadSlave[T, SingleToken[T]], expData: T, fire: Boolean=true, present: Boolean = true, now: Boolean = false)(implicit clk: Clock): Unit = {
    timescope {
        fork.withRegion(Monitor) {
          if (now) {
            c.resp.valid.expect(true.B)
          } else {
            while (!c.resp.valid.peekBoolean()) {
              clk.step()
            }
          }
          c.token.expect(true.B)
          c.present.expect(present.B)
          c.resp.bits.data.expect(expData)
        }.joinAndStep(clk)

      c.fire.poke(fire.B)
      clk.step()
    }
  }

  def writeMaster[T <: Data](c: TokenWriteSlave[T, SingleToken[T]], data: T, present: Boolean = true, fire: Boolean=true)(implicit clk: Clock): Unit = {
    timescope {
      fork.withRegion(Monitor) {
        while (!c.req.ready.peekBoolean()) {
          clk.step()
        }
      }.joinAndStep(clk)

      c.req.valid.poke(true.B)
      c.dat.valid.poke(true.B)
      c.dat.bits.data.poke(data)
      if (fire) {
        c.fire.poke(true.B)
      }
      clk.step()
    }
  }

  // FIXME: We also want to receive a bunch of writes and only expect on the last one (essentially emulate a connection)
  def writeSlave[T <: Data](c: TokenWriteMaster[T, SingleToken[T]], expData: T, present: Boolean = true, fire: Boolean = true, now: Boolean = false)(implicit clk: Clock): Unit = {
    timescope {
      c.req.ready.poke(true.B)
      c.dat.ready.poke(true.B)
      fork.withRegion(Monitor) {
        if (!now) {
          while (!c.dat.valid.peekBoolean()) {
            clk.step()
          }
        }

        c.dat.valid.expect(true.B)
        c.dat.bits.data.expect(expData)
      }.joinAndStep(clk)

      if (fire) {
        fork.withRegion(Monitor) {
          while (!c.fire.peekBoolean()) {clk.step()}
        }.joinAndStep(clk)
      }

      clk.step()
    }
  }
}
