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

class R1 extends Module {
  val in1 = Module(new InputPortArbiterSingleValue(
    InputPortArbiterConfig(
      defData,
      defToken,
      nReaders = 1
    )
  ))

  class TestReactorIO extends Bundle {
    val in1 = Vec(1, new SingleTokenReadMaster(defData))
  }

  val io = IO(new TestReactorIO)
  in1.io.outward <> io.in1(0)
}


class TestConnectionBuilder extends Module {
  class TestIO extends Bundle {
    val in1 = new SingleTokenReadMaster(defData)
    val in2 = new SingleTokenReadMaster(defData)
    val out = new SingleTokenWriteMaster(defData)
  }
  val io = IO(new TestIO)

  val out = Module(new OutputPortArbiterSingleValue(
    OutputPortArbiterConfig(
      defData, defToken,
      nWriters = 1
    )
  ))
  out.io.plugInwards()

  val in1 = Module(new InputPortArbiterSingleValue(
    InputPortArbiterConfig(
      defData, defToken,
      nReaders = 1
    )
  ))
  in1.io.plugInwards()

  val in2 = Module(new InputPortArbiterSingleValue(
    InputPortArbiterConfig(
      defData, defToken,
      nReaders = 1
    )
  ))
  in2.io.plugInwards()

  io.out <> out.io.outward
  io.in1 <> in1.io.outward
  io.in2 <> in2.io.outward

  val connBuilder = new SingleValueConnectionFactory(
    defData
  )

  connBuilder.addUpstream(out.io.outward)
  connBuilder.addDownstream(VecInit(in1.io.outward))
  connBuilder.addDownstream(VecInit(in2.io.outward))
  val conn = connBuilder.construct()
}

class TestConnection extends AnyFlatSpec with ChiselScalatestTester {

  def initClocks(c: SingleValueConnection[UInt], clk: Clock) = {
    c.io.write.req.initSource().setSourceClock(clk)
    c.io.write.dat.initSource().setSourceClock(clk)
    c.io.reads.foreach(_.req.initSource().setSourceClock(clk))
    c.io.reads.foreach(_.resp.initSink().setSinkClock(clk))
  }

  behavior of "SingleValueConnection"
  it should "read write simple" in {
    test(new SingleValueConnection(
      ConnectionConfig(
        defData, defToken,
        nChans = 1
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      initClocks(c, c.clock)

      c.io.write.dat.enqueueNow(defWr.Lit(_.data -> 13.U))
      c.io.write.fire.poke(true.B)
      c.io.reads(0).resp.expectDequeue(defRd.Lit(_.data -> 13.U))
      c.io.reads(0).fire.poke(true.B)
      c.clock.step()
      c.io.reads(0).resp.valid.expect(false.B)
    }
  }

  it should "Write and read absent 1" in {
    test(new SingleValueConnection(
      ConnectionConfig(
        defData, defToken,
        nChans = 1
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      initClocks(c, c.clock)
      c.io.write.fire.poke(true.B)
      c.clock.step()
      c.io.reads(0).token.expect(true.B)
      c.io.reads(0).present.expect(false.B)
      c.io.reads(0).fire.poke(true.B)
      c.clock.step()
      c.io.reads(0).token.expect(false.B)
    }
  }
  it should "Write and read absent 2" in {
    test(new SingleValueConnection(
      ConnectionConfig(
        defData, defToken,
        nChans = 1
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      initClocks(c, c.clock)
      c.io.write.fire.poke(true.B)
      c.io.write.absent.poke(true.B)
      c.clock.step()
      c.io.reads(0).token.expect(true.B)
      c.io.reads(0).present.expect(false.B)
      c.io.reads(0).fire.poke(true.B)
      c.clock.step()
      c.io.reads(0).token.expect(false.B)
    }
  }

  it should "Write and read multiple with absent" in {
    test(new SingleValueConnection(
      ConnectionConfig(
        defData, defToken,
        nChans = 8
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      initClocks(c, c.clock)
      c.io.write.fire.poke(true.B)
      c.io.write.absent.poke(true.B)
      c.clock.step()
      for (i <- 0 until 8) {
        c.io.reads(i).token.expect(true.B)
        c.io.reads(i).present.expect(false.B)
        c.io.reads(i).fire.poke(true.B)
        c.clock.step()
        c.io.reads(i).token.expect(false.B)
      }
      c.clock.step()


      c.io.write.dat.enqueueNow(defWr.Lit(_.data -> 13.U))
      c.io.write.fire.poke(true.B)
      c.io.reads(0).resp.expectDequeue(defRd.Lit(_.data -> 13.U))
      c.io.reads(0).fire.poke(true.B)
      c.clock.step()
      c.io.reads(0).resp.valid.expect(false.B)

    }
  }
  behavior of "ConnectionBuilder"
  it should "build without error" in {
    test(new TestConnectionBuilder).withAnnotations(Seq(WriteVcdAnnotation)) { c =>}
  }

}
