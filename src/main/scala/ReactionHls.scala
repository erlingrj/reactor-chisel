//package reactor
//
//import chisel3._
//import chisel3.util._
//
//
//abstract class ReactionHls(c: ReactionConfig) extends Reaction(c) {
//
//  def connectControlSignals(hlsCtrl: VitisHlsControlIO) = {
//    hlsCtrl.clk := clock
//    hlsCtrl.rst := !reactionEnable
//    hlsCtrl.start := reactionEnable
//  }
//
//  def makeControlAssertions(hlsCtrl: VitisHlsControlIO) = {
//    assert(!(reactionEnable && !RegNext(reactionEnable) && !RegNext(hlsCtrl.idle)), s"[ReactionHls.scala] hlsReaction started but not ready")
//  }
//
//  def connectInPort(hlsIn: VitisHlsInputPort, portIn: PortOutIO[Data]) = {
//    hlsIn.q0 := portIn.data
//    portIn.addr := hlsIn.address0
//    portIn.en := hlsIn.ce0
//    hlsIn.present := portIn.present
//  }
//
//  def connectOutPort(hlsOut: VitisHlsOutputPort, portOut: PortInIO[Data]) = {
//    portOut.addr := hlsOut.address0
//    portOut.en := hlsOut.we0
//    portOut.data := hlsOut.d0
//  }
//
//  def connectStatePort(hlsState: VitisHlsStateInOutPort, statePort: ReactorStateIO[Data]) = {
//    statePort.readPort.addr := hlsState.address0
//    statePort.readPort.en := hlsState.ce0
//    hlsState.q0 := statePort.readPort.addr
//
//    statePort.writePort.addr := hlsState.address1
//    statePort.writePort.en := hlsState.we1
//    hlsState.d1 := statePort.writePort.data
//  }
//
//  val blackBoxHls: BlackBoxVitisHls
//}
