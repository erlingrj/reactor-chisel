package reactor

import chisel3._
import chisel3.util._

import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import fpgatidbits.PlatformWrapper._

import TestPortAccess._


class TestReactorDMA extends AnyFlatSpec with ChiselScalatestTester {

  def initClocks(c: ReactorDMAWithMem): Unit = {
    c.io.writeStart.initSource().setSourceClock(c.clock)
    c.io.readStart.initSource().setSourceClock(c.clock)
  }

  def populateMem(c: ReactorDMAWithMem, vals: Seq[Int]): Unit = {
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

  def inspectMem(c: ReactorDMAWithMem, vals: Seq[Int]): Unit = {
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


  def startRead(c: ReactorDMAWithMem, present: Seq[Boolean], baseAddr: Int =0): Unit = {
    val p = c.io.readStart
    timescope {
      p.bits.present zip present foreach {case (port, pres) => port.poke(pres.B)}
      p.bits.baseAddr.poke(baseAddr.U)
      p.valid.poke(true.B)
      fork
        .withRegion(Monitor) {
          while (!p.ready.peekBoolean()) {
            c.clock.step(1)
          }
        }
        .joinAndStep(c.clock)
    }
  }

  def startWrite(c: ReactorDMAWithMem, present: Seq[Boolean], baseAddr: Int =0): Unit = {
    val p = c.io.writeStart
    timescope {
      p.bits.present zip present foreach {case (port, pres) => port.poke(pres.B)}
      p.bits.baseAddr.poke(baseAddr.U)
      p.valid.poke(true.B)
      fork
        .withRegion(Monitor) {
          while (!p.ready.peekBoolean()) {
            c.clock.step(1)
          }
        }
        .joinAndStep(c.clock)
    }
  }

  val in1 = PortIOConfig( nElems = 8, gen = UInt(8.W))
  val in2 = PortIOConfig( nElems = 8, gen = UInt(8.W))
  val out1 = PortIOConfig(nElems = 8, gen = UInt(8.W))
  val out2 = PortIOConfig(nElems = 8, gen = UInt(8.W))

  val cfg = ReactorDMAConfig(
    inPorts = Array(in1,in2),
    outPorts = Array(out1, out2),
    mrp = TesterWrapperParams.toMemReqParams()
  )
  val in3 = PortIOConfig(nElems = 4, gen = UInt(8.W))
  val in4 = PortIOConfig(nElems = 5, gen = UInt(7.W))
  val out3 = PortIOConfig(nElems = 6, gen = UInt(6.W))
  val out4 = PortIOConfig(nElems = 7, gen = UInt(5.W))

  val cfg2 = ReactorDMAConfig(
    inPorts = Array(in3,in4),
    outPorts = Array(out3, out4),
    mrp = TesterWrapperParams.toMemReqParams()
  )
  behavior of "ReactorDMA"

  it should "initialize" in {
    test(new ReactorDMAWithMem(cfg)) { c =>
      initClocks(c)
      c.io.readStart.ready.expect(true.B)
    }
  }

  it should "Read the right data from mem" in {
    test(new ReactorDMAWithMem(cfg)) { c =>
      initClocks(c)
      val mem1 = (10 to 17)
      val mem2 = (18 to 25)
      populateMem(c, mem1 ++ mem2)

      fork {
        startRead(c,Seq(true,true))
      }. fork {
        expWrite(c.io.portWrite(0), c.clock, mem1)
      }. fork {
        expWrite(c.io.portWrite(1), c.clock, mem2)
      }.join()
    }
  }

  it should "Read the right data from mem and finish" in {
    test(new ReactorDMAWithMem(cfg)) { c =>
        initClocks(c)
        val mem1 = (10 to 17)
        val mem2 = (18 to 25)
        populateMem(c, mem1 ++ mem2)

        fork {
          startRead(c,Seq(true,true))
        }. fork {
          expWrite(c.io.portWrite(0), c.clock, mem1)
        }. fork {
          expWrite(c.io.portWrite(1), c.clock, mem2)
        }.join()

      while(!c.io.readDone.peekBoolean()) {
        c.clock.step()
      }
    }
  }

  it should "read data from different width and size ports" in {
    test(new ReactorDMAWithMem(cfg2)) { c =>
      initClocks(c)
      val mem1 = (10 to 13)
      val mem2 = (20 to 24)
      populateMem(c, mem1 ++ mem2)

      fork {
        startRead(c,Seq(true,true))
      }. fork {
        expWrite(c.io.portWrite(0), c.clock, mem1)
      }. fork {
        expWrite(c.io.portWrite(1), c.clock, mem2)
      }.join()

      while(!c.io.readDone.peekBoolean()) {
        c.clock.step()
      }
    }
  }

  it should "Write the right data from mem" in {
    test(new ReactorDMAWithMem(cfg)) { c =>
      initClocks(c)
      val mem1 = (10 to 17)
      val mem2 = (18 to 25)

      fork {
        startWrite(c, Seq(true,true))
      }. fork {
        expRead(c.io.portRead(1), c.clock, mem2)
      }. fork {
        expRead(c.io.portRead(0), c.clock, mem1)
      }.join()

      while(!c.io.writeDone.peekBoolean()) {
        c.clock.step()
      }
      inspectMem(c, mem1++mem2)
    }
  }
  it should "Read and write data and finish" in {
    test(new ReactorDMAWithMem(cfg)) { c =>
      initClocks(c)
      val mem1 = (10 to 17)
      val mem2 = (18 to 25)
      populateMem(c, mem1 ++ mem2)
      fork {
        startRead(c, Seq(true,true))
      }. fork {
        expWrite(c.io.portWrite(0), c.clock, mem1)
      }.fork{
        expWrite(c.io.portWrite(1), c.clock, mem2)
      }. join()

      while(!c.io.readDone.peekBoolean()) {
        c.clock.step()
      }

      fork {
        startWrite(c, Seq(true,true))
      }. fork {
        expRead(c.io.portRead(0), c.clock, mem1)
      }. fork {
        expRead(c.io.portRead(1), c.clock, mem2)
      }.join()

      while(!c.io.writeDone.peekBoolean()) {
        c.clock.step()
      }
      inspectMem(c, mem1 ++ mem2)

    }
  }

  it should "Read and write multiple data and finish" in {
    test(new ReactorDMAWithMem(cfg)) { c =>
      initClocks(c)
      val mem = Seq(
        Seq(10 until 18, 20 until 28),
        Seq(30 until 38, 40 until 48),
        Seq(50 until 58, 60 until 68),
      )
      for (m <- mem) {
        populateMem(c, m(0) ++ m(1))
        fork {
          startRead(c, Seq(true,true))
        }. fork {
          expWrite(c.io.portWrite(0), c.clock, m(0))
        }. fork {
          expWrite(c.io.portWrite(1), c.clock, m(1))
        }. join()

        while(!c.io.readDone.peekBoolean()) {
          c.clock.step()
        }

        fork {
          startWrite(c, Seq(true,true))
        }. fork {
          expRead(c.io.portRead(0), c.clock, m(0))
        }. fork {
          expRead(c.io.portRead(1), c.clock, m(1))
        }.join()
        while(!c.io.writeDone.peekBoolean()) {
          c.clock.step()
        }
        inspectMem(c, m(0)++m(1))
      }
      }
  }

  it should "Read and write data with multiple lenghts and finish" in {
    test(new ReactorDMAWithMem(cfg2)) { c =>
      initClocks(c)

      val mem1 = 10 until 14
      val mem2 = 20 until 25
      val mem3 = 30 until 36
      val mem4 = 1 until 8

      populateMem(c, mem1++mem2)
      fork {
        startRead(c, Seq(true, true))
      }. fork {
        expWrite(c.io.portWrite(0), c.clock, mem1)
      }. fork {
        expWrite(c.io.portWrite(1), c.clock, mem2)
      }. join()

      while(!c.io.readDone.peekBoolean()) {
        c.clock.step()
      }

      fork {
        startWrite(c, Seq(true, true))
      }. fork {
        expRead(c.io.portRead(0), c.clock, mem3)
      }. fork {
        expRead(c.io.portRead(1), c.clock, mem4)
      }.join()
      while(!c.io.writeDone.peekBoolean()) {
        c.clock.step()
      }
      inspectMem(c, mem3++mem4)
    }
  }

  it should "read and write only some ports" in {
    test(new ReactorDMAWithMem(cfg)) { c =>
      initClocks(c)
      val mem1 = (10 to 17)
      val mem2 = (18 to 25)
      populateMem(c, mem1++mem2)
      fork {
        startRead(c, Seq(false,true))
      }.fork{
        expWrite(c.io.portWrite(1), c.clock, mem2)
      }. join()

      while(!c.io.readDone.peekBoolean()) {
        c.clock.step()
      }

      fork {
        startWrite(c, Seq(false,true))
      }. fork {
        expRead(c.io.portRead(1), c.clock, mem1)
      }.join()

      while(!c.io.writeDone.peekBoolean()) {
        c.clock.step()
      }
      inspectMem(c, mem1 ++ mem1)
    }
  }
}
