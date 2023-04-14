package reactor

import chisel3._
import chisel3.util._


case class InputPortConfig[T <: BaseEvent] (
                                   gen: T,
                                   nReaders: Int
                                 )
class InputPortIO[T <: BaseEvent](c: InputPortConfig[T]) extends Bundle {
  val inward = Vec(c.nReaders, c.gen)
  val outward = Flipped(c.gen)

  def driveDefaults() = {
    inward.foreach(_.driveDefaults)
    outward.driveDefaultsFlipped
  }

  def driveDefaultsFlipped() = {
    inward.foreach(_.driveDefaultsFlipped)
    outward.driveDefaults
  }
}

class InputPort[T <: BaseEvent](c: InputPortConfig[T]) extends Module {
  val io = IO(new InputPortIO(c))
  assert(!(io.outward.fire && !io.outward.valid), "[Event.scala] Reader asserted `fire` while `valid` was false")
  io.driveDefaults()



  // Connect the readers on the inward ports to the outwards port in turn
//  val regCnt = RegInit(0.U(log2Ceil(c.nReaders).W))
//  io.outward <> io.inward(regCnt)
//  when (io.outward.fire) {
//    when(regCnt === (c.nReaders-1).U) {
//      regCnt := 0.U
//    }.otherwise {
//      regCnt := regCnt + 1.U
//    }
//  }
}

case class OutputPortConfig[T <: BaseEvent](
                                      gen: T,
                                      nWriters: Int
                                      )
class OutputPortIO[T <: BaseEvent](c: OutputPortConfig[T]) extends Bundle {
  val inward = Vec(c.nWriters, Flipped(c.gen))
  val outward = c.gen.gen

  def driveDefaults() = {
    inward.foreach(_.driveDefaultsFlipped)
    outward.driveDefaults
  }
  def driveDefaultsFlipped() = {
    inward.foreach(_.driveDefaults)
    outward.driveDefaultsFlipped
  }
}

class OutputPort[T <: BaseEvent](c: OutputPortConfig[T]) extends Module {
  val io = IO(new OutputPortIO(c))
  io.drive
}
