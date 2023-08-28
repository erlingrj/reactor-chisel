package reactor

import chisel3._
import chisel3.util.DecoupledIO

object ReactionApi {

  /**
   * Write data to a normal output port
   *
   * @param port
   * @param data
   * @param reaction
   * @tparam T1
   * @tparam T2
   */
  def lf_set[T1 <: Data, T2 <: Token[T1]](port: TokenWriteMaster[T1, T2], data: T1)(implicit reaction: Reaction): DecoupledIO[TokenWrDat[T1]] = {
    port.write(data)
  }

  def lf_set[T1 <: Data, T2 <: Token[T1]](port: TokenWriteMaster[T1, T2], addr: UInt, size: UInt, data: T1)(implicit reaction: Reaction): DecoupledIO[TokenWrDat[T1]] = {
    port.write(addr, size)
  }

  /**
   * Write data to an external output port
   * @param port
   * @param data
   * @param reaction
   * @tparam T
   */
  def lf_set[T <: Data](port: StateReadWriteMaster[T, SingleToken[T]], data: T)(implicit reaction: Reaction): Unit = {
    port.write.write(data)
  }


  def lf_present[T1 <: Data, T2 <: Token[T1]](port: TokenReadMaster[T1,T2]): Bool= {
    port.present
  }

  /**
   * Get data from a normal input port
   *
   * @param port
   * @tparam T1
   * @tparam T2
   * @return
   */
  def lf_get[T1 <: Data, T2 <: Token[T1]](port: TokenReadMaster[T1, T2]): DecoupledIO[TokenRdResp[T1]] = {
    port.read
  }

  def lf_get[T1 <: Data, T2 <: Token[T1]](port: TokenReadMaster[T1, T2], addr: UInt, size: UInt): DecoupledIO[TokenRdResp[T1]] = {
    port.read(addr, size)
  }

  /**
   * Get data from an external input port
   * @param port
   * @tparam T1
   * @return
   */
  def lf_get[T1 <: Data](port: T1): T1 = {
    port
  }

  def lf_get[T <: Data](port: StateReadWriteMaster[T, SingleToken[T]])(implicit reaction: Reaction): T = {
    port.read.read()
  }

  /**
   * Read from a state variable
   * @param state
   * @tparam T1
   * @tparam T2
   * @return
   */
  def lf_read[T1 <: Data, T2 <: Token[T1]](state: StateReadWriteMaster[T1,T2]): T1 = {
    state.read.read()
  }

  /**
   * Write to a state variable
   * @param state
   * @param data
   * @tparam T1
   * @tparam T2
   */
  def lf_write[T1 <: Data, T2 <: Token[T1]](state: StateReadWriteMaster[T1, T2], data: T1): Unit = {
    state.write.write(data)
  }
  def lf_time_logical()(implicit reaction: Reaction): UInt = reaction.logicalTag
  def lf_time_physical()(implicit reaction: Reaction): UInt = reaction.physicalTag

  def lf_print(toPrint: Printable): Unit = {
    printf(toPrint)
  }
}
