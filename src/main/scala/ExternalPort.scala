package reactor
import chisel3._

/**
 *  This file contains utilities for external ports. That is special ports of the reactor which are not connected
 *  to other reactors but rather brought up to the top-level reactor and exposed, e.g. to pins of the FPGA.
 */

class ExternalOutputPortLatchIO[T1 <: Data](genData: T1, nWriters: Int) extends Bundle {
  val write = Vec(nWriters, new StateReadWriteSlave(genData, new SingleToken(genData)))
  val read = Output(genData)
}

/**
 *  The ExternalOutputPortLatch is a "latch" which saves the value with which to drive the external pin.
 *  The problem is that if we "just" connect ttthe output pin directly to the Reaction module, what value should
 *  it take between reaction invokations and what about the scenario with multiple reactions driving the port.
 *  It was easier to use the State variable modules to create a latch for this.
 * @param genData
 * @param nWriters
 * @tparam T1
 */
class ExternalOutputPortLatch[T1 <: Data](genData: T1, nWriters: Int) extends Module {
  val io = IO(new ExternalOutputPortLatchIO(genData, nWriters))

  val latch = Module(new SingleValueState(StateConfig(
    genData = genData,
    genToken = new SingleToken(genData),
    nReactions = nWriters + 1,
    protocol = Immediate
  )))

  for (i <- 0 until nWriters) {
    latch.io.ports(i) <> io.write(i)
  }

  latch.io.ports.last.write.driveDefaultsFlipped()
  latch.io.ports.last.read.driveDefaultsFlipped()
  io.read := latch.io.ports.last.read.resp.data

  var nAttchedWriters = 0
  def <>(reaction: StateReadWriteMaster[T1, SingleToken[T1]]) = {
    require(nWriters > nAttchedWriters, s"[ExternalPort.scala] Tried connecting more reactions to OutputLatch, but only ${nWriters} reactions specified in the config")
    io.write(nAttchedWriters) <> reaction
    nAttchedWriters += 1
  }
}
