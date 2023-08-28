package reactor

import chisel3._
import chisel3.util._

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
  def lf_set[T1 <: Data](port: TokenWriteMaster[T1, SingleToken[T1]], data: T1)(implicit reaction: Reaction): Unit = {
    port.dat.valid := true.B
    port.dat.bits.data := data
    assert(port.dat.ready)
  }

  // FIXME: It would be nice to not have bits.data I.e. return DecoupledIO[T1] not the ArrayTokenRdResp...
  def lf_set_array[T1 <: Data](port: ArrayTokenWriteMaster[T1], addr: UInt, size: UInt)(implicit reaction: Reaction): DecoupledIO[TokenWrDat[T1]] = {
    val sIdle :: sReading :: Nil = Enum(2)
    val regState = RegInit(sIdle)

    switch(regState) {
      is(sIdle) {
        port.req.bits.addr := addr
        port.req.bits.size := size
        port.req.valid := true.B

        when(port.req.fire) {
          regState := sReading
        }
      }
      is(sReading) {}
    }
    port.dat
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
  def lf_get[T1 <: Data](port: TokenReadMaster[T1, SingleToken[T1]]): T1 = {
    assert(port.token && port.present && port.resp.valid)
    port.resp.ready := true.B
    port.resp.bits.data
  }

  def lf_get_array[T1 <: Data](port: ArrayTokenReadMaster[T1], addr: UInt, size: UInt)(implicit reaction: Reaction): DecoupledIO[TokenRdResp[T1]] = {

    val sIdle :: sReading :: Nil = Enum(2)
    val regState = RegInit(sIdle)

    switch(regState) {
      is(sIdle) {
        port.req.bits.addr := addr
        port.req.bits.size := size
        port.req.valid := true.B

        when(port.req.fire) {
          regState := sReading
        }
      }
      is(sReading) {}
    }
    port.resp
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
