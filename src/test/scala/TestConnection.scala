package reactor.test

import reactor._
import reactor.globals._

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.VecLiterals._


// This object implements convenience functions for simulating a Connection and writing and reading and expecting
object ConnectionSim {
  def write[T <: Data](c: EventWriteSlave[T,SingleToken[T]], data: T, fire: Boolean)(implicit clk: Clock): Unit = {
    timescope {
      c.req.valid.poke(true.B)
      c.req.present.poke(true.B)
      c.req.token.data.poke(data)
      if (fire) {
        c.fire.poke(true.B)
      } else {
        c.fire.poke(false.B)
      }
      clk.step()
    }
  }

  def writeAbsent[T <: Data](c: EventWriteSlave[T, SingleToken[T]], fire: Boolean)(implicit clk: Clock): Unit = {
    timescope {
      c.req.valid.poke(true.B)
      c.req.present.poke(false.B)
      if (fire) {
        c.fire.poke(true.B)
      } else {
        c.fire.poke(false.B)
      }
      clk.step()
    }
  }

  def readNow[T <: Data](c: EventReadSlave[T, SingleToken[T]], data: T, fire: Boolean)(implicit clk: Clock): Unit = {
    timescope {
      c.resp.valid.expect(true.B)
      c.resp.present.expect(true.B)
      c.resp.token.data.expect(data)

      if (fire) {
        c.fire.poke(true.B)
      } else {
        c.fire.poke(false.B)
      }
      clk.step()
    }
  }
  def readAbsentNow[T <: Data](c: EventReadSlave[T, SingleToken[T]], fire: Boolean)(implicit clk: Clock): Unit = {
    timescope {
      c.resp.valid.expect(true.B)
      c.resp.present.expect(false.B)

      if (fire) {
        c.fire.poke(true.B)
      } else {
        c.fire.poke(false.B)
      }
      clk.step()
    }
  }
}

class R1 extends Module {
  val in1 = Module(new InputPort(
    InputPortConfig(
      defData,
      defToken,
      nReaders = 1
    )
  ))

  class TestReactorIO extends Bundle {
    val in1 = Vec(1, new EventReadMaster(defData, defToken))
  }

  val io = IO(new TestReactorIO)
  in1.io.outward <> io.in1(0)
}


class TestConnectionBuilder extends Module {
  class TestIO extends Bundle {
    val in1 = new EventReadMaster(defData, defToken)
    val in2 = new EventReadMaster(defData, defToken)
    val out = new EventWriteMaster(defData, defToken)
  }
  val io = IO(new TestIO)

  val out = Module(new OutputPort(
    OutputPortConfig(
      defData, defToken,
      nWriters = 1
    )
  ))
  out.io.plugInwards()

  val in1 = Module(new InputPort(
    InputPortConfig(
      defData, defToken,
      nReaders = 1
    )
  ))
  in1.io.plugInwards()

  val in2 = Module(new InputPort(
    InputPortConfig(
      defData, defToken,
      nReaders = 1
    )
  ))
  in2.io.plugInwards()

  io.out <> out.io.outward
  io.in1 <> in1.io.outward
  io.in2 <> in2.io.outward

  val connFunc = (cfg: ConnectionConfig[UInt, SingleToken[UInt]]) => new SingleValueConnection(cfg)
  val connBuilder = new ConnectionFactory(connFunc, defData, defToken)

  connBuilder.addUpstream(out.io.outward)
  connBuilder.addDownstream(VecInit(in1.io.outward))
  connBuilder.addDownstream(VecInit(in2.io.outward))
  val conn = connBuilder.construct()
}

class TestConnection extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SingleValueConnection"
  it should "read write simple" in {
    test(new SingleValueConnection(
      ConnectionConfig(
        defData, defToken,
        nChans = 1
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val clk = c.clock
      ConnectionSim.write(c.io.write, 13.U, true)
      c.clock.step()
      ConnectionSim.readNow(c.io.reads(0), 13.U,true)
      c.clock.step()
      c.io.reads(0).resp.valid.expect(false.B)
    }
  }

  it should "Write and read absent" in {
    test(new SingleValueConnection(
      ConnectionConfig(
        defData, defToken,
        nChans = 1
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val clk = c.clock
      ConnectionSim.writeAbsent(c.io.write, true)
      c.clock.step()
      ConnectionSim.readAbsentNow(c.io.reads(0), true)
    }
  }

  it should "Write and read multiple with absent" in {
    test(new SingleValueConnection(
      ConnectionConfig(
        defData, defToken,
        nChans = 8
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      implicit val clk = c.clock
      ConnectionSim.writeAbsent(c.io.write, true)
      c.clock.step()
      for (i <- 0 until 8) {
        ConnectionSim.readAbsentNow(c.io.reads(i), true)
      }
      c.io.reads(0).resp.valid.expect(false.B)

      ConnectionSim.write(c.io.write, 21.U, false)
      c.io.reads(0).resp.valid.expect(false.B)
      ConnectionSim.write(c.io.write, 42.U, true)

      for (i <- 0 until 8) {
        ConnectionSim.readNow(c.io.reads(i), 42.U, true)
      }
      c.io.reads(0).resp.valid.expect(false.B)
    }
  }
  behavior of "ConnectionBuilder"
  it should "build without error" in {
    test(new TestConnectionBuilder).withAnnotations(Seq(WriteVcdAnnotation)) { c =>}
  }

}
