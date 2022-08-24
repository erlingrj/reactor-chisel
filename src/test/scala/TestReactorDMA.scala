package reactor

import chisel3._
import chisel3.util._

import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

import fpgatidbits.PlatformWrapper._

import TestPortAccess._


class TestReactorDMA extends AnyFlatSpec with ChiselScalatestTester {

  def initClocks(c: ReactorDMAWithMem[UInt,UInt]): Unit = {
    c.io.writeStart.initSource().setSourceClock(c.clock)
    c.io.readStart.initSource().setSourceClock(c.clock)
  }

  def populateMem(c: ReactorDMAWithMem[UInt, UInt], vals: Seq[Int]): Unit = {
    var addr = 0
    timescope {
      for (v <- vals) {
        c.ioMem.memAddr.poke(addr.U)
        c.ioMem.memWriteEn.poke(true.B)
        c.ioMem.memWriteData.poke(v)
        c.clock.step()
        addr += 8
      }
    }
  }

  def inspectMem(c: ReactorDMAWithMem[UInt, UInt], vals: Seq[Int]): Unit = {
    var addr = 0
    timescope {
      for (v <- vals) {
        c.ioMem.memAddr.poke(addr.U)
        c.ioMem.memWriteEn.poke(false.B)
        c.clock.step()
        c.ioMem.memReadData.expect(v.U)
        addr += 8
      }
    }
  }


  def startRead(c: ReactorDMAWithMem[UInt, UInt], byteCount: Int, baseAddr: Int =0): Unit = {
    c.io.readStart.enqueue(chiselTypeOf(c.io.readStart).bits.Lit(
      _.baseAddr -> baseAddr.U,
      _.byteCount -> byteCount.U
    ))
  }


  def startWrite(c: ReactorDMAWithMem[UInt, UInt], byteCount: Int, baseAddr: Int =0): Unit = {
    c.io.writeStart.enqueue(chiselTypeOf(c.io.readStart).bits.Lit(
      _.baseAddr -> baseAddr.U,
      _.byteCount -> byteCount.U
    ))
  }

  val c = ReactorDMAConfig(
    nElemsIn = 4,
    nElemsOut = 4,
    mrp = TesterWrapperParams.toMemReqParams()
  )
  val genIn = UInt(8.W)
  val genOut = UInt(8.W)

  val c2 = ReactorDMAConfig(
    nElemsIn = 6,
    nElemsOut = 3,
    mrp = TesterWrapperParams.toMemReqParams()
  )


  behavior of "ReactorDMA"

  it should "initialize" in {
    test(new ReactorDMAWithMem(c,genIn,genOut)) { c =>
      initClocks(c)
      c.io.readStart.ready.expect(true.B)
    }
  }
  it should "Read the right data from mem" in {
    test(new ReactorDMAWithMem(c,genIn,genOut)) { c =>
      initClocks(c)
      val mem = Seq(10,11,12,13)
      populateMem(c, mem)

      fork {
        startRead(c, 4 * 8)
      }. fork {
        expWrite(c.io.portWrite, c.clock, mem)
      }. join()
    }
  }

  it should "Read the right data from mem and finish" in {
    test(new ReactorDMAWithMem(c,genIn,genOut)) { c =>
      initClocks(c)
      val mem = Seq(101,102,103,104)
      populateMem(c, mem)
      fork {
        startRead(c, 4 * 8)
      }. fork {
        expWrite(c.io.portWrite, c.clock, mem)
      }. join()

      while(!c.io.readDone.peekBoolean()) {
        c.clock.step()
      }
    }
  }

  it should "Write the right data from mem" in {
    test(new ReactorDMAWithMem(c,genIn,genOut)) { c =>
      initClocks(c)
      val mem = Seq(10,11,12,13)
      fork {
        startWrite(c, 4 * 8)
      }. fork {
        expRead(c.io.portRead, c.clock, mem)
      }.join()
      while(!c.io.writeDone.peekBoolean()) {
        c.clock.step()
      }
      inspectMem(c, mem)
    }
  }

  it should "Read and write data and finish" in {
    test(new ReactorDMAWithMem(c,genIn,genOut)) { c =>
      initClocks(c)
      val mem = Seq(101,102,103,104)
      populateMem(c, mem)
      fork {
        startRead(c, 4 * 8)
      }. fork {
        expWrite(c.io.portWrite, c.clock, mem)
      }. join()

      while(!c.io.readDone.peekBoolean()) {
        c.clock.step()
      }

      fork {
        startWrite(c, 4 * 8)
      }. fork {
        expRead(c.io.portRead, c.clock, mem)
      }.join()
      while(!c.io.writeDone.peekBoolean()) {
        c.clock.step()
      }
      inspectMem(c, mem)

    }
  }

  it should "Read and write multiple data and finish" in {
    test(new ReactorDMAWithMem(c,genIn,genOut)) { c =>
      initClocks(c)
      val mem = Seq(
        Seq(101,102,103,104),
        Seq(200,201,202,203),
        Seq(70,71,72,73)
      )
      for (m <- mem) {

        populateMem(c, m)
        fork {
          startRead(c, 4 * 8)
        }. fork {
          expWrite(c.io.portWrite, c.clock, m)
        }. join()

        while(!c.io.readDone.peekBoolean()) {
          c.clock.step()
        }

        fork {
          startWrite(c, 4 * 8)
        }. fork {
          expRead(c.io.portRead, c.clock, m)
        }.join()
        while(!c.io.writeDone.peekBoolean()) {
          c.clock.step()
        }
        inspectMem(c, m)

      }
      }
  }

  it should "Read and write data with multiple lenghts and finish" in {
    test(new ReactorDMAWithMem(c2,genIn,genOut)) { c =>
      initClocks(c)
      val memIn = Seq(101,102,103,104,105,106)
      val memOut = Seq(201,202,203)

      populateMem(c, memIn)
      fork {
        startRead(c, 6 * 8)
      }. fork {
        expWrite(c.io.portWrite, c.clock, memIn)
      }. join()

      while(!c.io.readDone.peekBoolean()) {
        c.clock.step()
      }

      fork {
        startWrite(c, 3 * 8)
      }. fork {
        expRead(c.io.portRead, c.clock, memOut)
      }.join()
      while(!c.io.writeDone.peekBoolean()) {
        c.clock.step()
      }
      inspectMem(c, memOut)
    }
  }

}
