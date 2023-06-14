package reactor

import chisel3._

object ReactionApi {

  def lf_set[T1 <: Data, T2 <: Token[T1]](port: EventWriteMaster[T1, T2], data: T1): Unit = {
    port.write(data)
  }
  def lf_present[T1 <: Data, T2 <: Token[T1]](port: EventReadMaster[T1,T2]): Bool= {
    port.resp.present
  }
  def lf_get[T1 <: Data, T2 <: Token[T1]](port: EventReadMaster[T1, T2]): T1 = {
    port.read()
  }

  def lf_read[T1 <: Data, T2 <: Token[T1]](state: StateReadWriteMaster[T1,T2]): T1 = {
    state.read.read()
  }

  def lf_write[T1 <: Data, T2 <: Token[T1]](state: StateReadWriteMaster[T1, T2], data: T1): Unit = {
    state.write.write(data)
  }

  def lf_time_logical()(implicit reaction: Reaction): UInt = reaction.logicalTag
  def lf_time_physical()(implicit reaction: Reaction): UInt = reaction.physicalTag
}
