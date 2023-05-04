package reactor.examples

import chisel3._
import reactor._
import fpgatidbits.PlatformWrapper._

class TopReactorExIO extends MainReactorIO(TesterWrapperParams) {
  val in = Input(new SwSingleToken(UInt(8.W)))
  val out1 = Output(new SwSingleToken(UInt(8.W)))
  val out2 = Output(new SwSingleToken(UInt(8.W)))
}
class TopReactorEx extends MainReactor(TesterWrapperParams) {
  val accelParams = AcceleratorParams(0)
  val gen = new SingleToken(UInt(8.W))

  // Top-level, software-facing IO
  val io = IO(new TopReactorExIO())
  io.signature := makeDefaultSignature()

  // Contained Reactors
  val c1 = Module(new DualWithContained)
  val c2 = Module(new DualAddN)
  val c3 = Module(new DualAddN)

  // Scheduler
  val scheduler = Module(new Scheduler(SchedulerConfig(nTopInputPorts = 1, nTopOutputPorts = 2)))
  io.connectScheduler(scheduler)

  // Top-level ports
  val in = Module(new TopInputPort(TopInputPortConfig(new SwSingleToken(UInt(8.W)))))
  val out1 = Module(new TopOutputPort(TopOutputPortConfig(new SwSingleToken(UInt(8.W)))))
  val out2 = Module(new TopOutputPort(TopOutputPortConfig(new SwSingleToken(UInt(8.W)))))

  // Connect top-level ports to scheduler
  scheduler.connect(in)
  scheduler.connect(out1)
  scheduler.connect(out2)

  // Connect top-level ports to top-level IO
  in.io.in <> io.in
  out1.io.out <> io.out1
  out2.io.out <> io.out2

  // Connections
  val c_in = new ConnectionBuilder(
    (c: ConnectionConfig[SingleToken[UInt]]) => new SingleValueConnection(c),
    new SingleToken(UInt(8.W))
  )
  c_in.addUpstream(in.io.out)
  c_in.addDownstream(c1.io.in)
  c_in.construct()

  val c_c1_c2 = new ConnectionBuilder(
    (c: ConnectionConfig[SingleToken[UInt]]) => new SingleValueConnection(c),
    new SingleToken(UInt(8.W))
  )
  c_c1_c2.addUpstream(c1.io.out1)
  c_c1_c2.addDownstream(c2.io.in)
  c_c1_c2.construct()

  val c_c1_c3 = new ConnectionBuilder(
    (c: ConnectionConfig[SingleToken[UInt]]) => new SingleValueConnection(c),
    new SingleToken(UInt(8.W))
  )
  c_c1_c3.addUpstream(c1.io.out2)
  c_c1_c3.addDownstream(c3.io.in)
  c_c1_c3.construct()

  val c_out1 = new ConnectionBuilder(
    (c: ConnectionConfig[SingleToken[UInt]]) => new SingleValueConnection(c),
    new SingleToken(UInt(8.W))
  )
  c_out1.addUpstream(c2.io.out)
  c_out1.addDownstream(out1.io.in)
  c_out1.construct()

  val c_out2 = new ConnectionBuilder(
    (c: ConnectionConfig[SingleToken[UInt]]) => new SingleValueConnection(c),
    new SingleToken(UInt(8.W))
  )
  c_out2.addUpstream(c3.io.out)
  c_out2.addDownstream(out2.io.in)
  c_out2.construct()
}
