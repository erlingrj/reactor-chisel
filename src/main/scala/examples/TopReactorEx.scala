package reactor.examples

import chisel3._
import reactor._
import reactor.globals._

import fpgatidbits.PlatformWrapper._

class TopReactorExIO extends CodesignMainReactorIO(TesterWrapperParams) {
  val in = Input(new SwSingleToken(defData))
  val out1 = Output(new SwSingleToken(defData))
  val out2 = Output(new SwSingleToken(defData))
}
class TopReactorEx extends CodesignMainReactor(TesterWrapperParams) {
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
  val cfg = TopInputPortConfig(defData, new SwSingleToken(defData))

  val in = Module(new TopInputPort(TopInputPortConfig(defData, new SwSingleToken(defData))))
  val out1 = Module(new TopOutputPort(TopOutputPortConfig(defData, new SwSingleToken(defData))))
  val out2 = Module(new TopOutputPort(TopOutputPortConfig(defData, new SwSingleToken(defData))))

  // Connect top-level ports to scheduler
  scheduler.connect(in)
  scheduler.connect(out1)
  scheduler.connect(out2)

  // Connect top-level ports to top-level IO
  in.io.in <> io.in
  out1.io.out <> io.out1
  out2.io.out <> io.out2

  // Connections
  val c_in = new ConnectionFactory(
    (c: ConnectionConfig[UInt, SingleToken[UInt]]) => new SingleValueConnection(c), defData, defToken
  )

  c_in.addUpstream(in.io.out)
  c_in.addDownstream(c1.io.in)
  c_in.construct()

  val c_c1_c2 = new ConnectionFactory(
    (c: ConnectionConfig[UInt, SingleToken[UInt]]) => new SingleValueConnection(c),
    defData, defToken,
  )
  c_c1_c2 << c1.io.out1
  c_c1_c2 >> c2.io.in
  c_c1_c2.construct()

  val c_c1_c3 = new ConnectionFactory(
    (c: ConnectionConfig[UInt, SingleToken[UInt]]) => new SingleValueConnection(c),
    defData, defToken
  )
  c_c1_c3 << c1.io.out2
  c_c1_c3 >> c3.io.in
  c_c1_c3.construct()

  val c_out1 = new ConnectionFactory(
    (c: ConnectionConfig[UInt, SingleToken[UInt]]) => new SingleValueConnection(c),
    defData, defToken
  )

  c_out1 << c2.io.out
  c_out1 >> out1.io.in
  c_out1.construct()

  val c_out2 = new ConnectionFactory(
    (c: ConnectionConfig[UInt, SingleToken[UInt]]) => new SingleValueConnection(c),
    defData, defToken
  )
  c_out2 << c3.io.out
  c_out2 >> out2.io.in
  c_out2.construct()
}
