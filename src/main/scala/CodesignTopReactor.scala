package reactor

import chisel3._
import fpgatidbits.PlatformWrapper._
abstract class CodesignTopReactorIO(p: PlatformWrapperParams) extends GenericAcceleratorIF(p.numMemPorts,p) {
  val start = Input(Bool())
  val done = Output(Bool())
  val running = Output(Bool())

  def connectScheduler(s: Scheduler): Unit = {
    s.io.start := start
    running := s.io.running
    done := s.io.done
  }
}

abstract class CodesignTopReactor(p: PlatformWrapperParams) extends GenericAccelerator(p) {

  val numMemPorts = p.numMemPorts
  val io: CodesignTopReactorIO
  val scheduler: Scheduler

}
