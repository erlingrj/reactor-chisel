package reactor

import chisel3._


abstract class ReactorIO extends Bundle {}

abstract class Reactor extends Module {

  val io: ReactorIO
  val reactions: Seq[Reaction] = Seq()
  val inPorts: Seq[InputPort[_ <: Token]] = Seq()
  val outPorts: Seq[OutputPort[_ <: Token]] = Seq()
  val connections: Seq[Connection[_ <: Token]] = Seq()
  val childReactors: Seq[Reactor] = Seq()

}

class ReactorIoBuilder
//abstract class ReactorIO extends Bundle {
//  def ins: Seq[InputPortIO[_ <: BaseEventIO]]
//  def outs: Seq[OutputPortIO[_ <: BaseEventIO]]
//}
//
//abstract class Reactor extends Module {
//  val inPorts: Array[InputPort[BaseEventIO]]
//  val outPorts: Array[OutputPort[BaseEventIO]]
//}
//
//
//
//class ExampleReactorIO extends ReactorIO {
//  val in1 = new InputPortIO(InputPortConfig(gen=new SingleValueEventIO(gen=UInt(8.W)), nReaders=1))
//  val in2 = new InputPortIO(InputPortConfig(gen=new BramArrayEventIO(gen=UInt(8.W),depth=8), nReaders=1))
//  val out1 = new OutputPortIO(OutputPortConfig(gen=new SingleValueEventIO(gen=UInt(8.W)), nWriters = 1))
//
//  def ins = Seq(in1, in2)
//  def outs = Seq(out1)
//
//
//
//}
//
