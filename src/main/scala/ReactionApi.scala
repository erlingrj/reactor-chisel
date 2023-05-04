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

}