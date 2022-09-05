package reactor

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror.directionOf



case class ReactionConfig(
  triggers : Array[PortIOConfig[Data]],
  antiDependencies : Array[PortIOConfig[Data]],
  dependencies : Array[PortIOConfig[Data]]
)


class ReactionCtrlIO extends Bundle {
  // Control signals
  val done = Output(Bool())
  val running = Output(Bool())
  val enable = Flipped(Decoupled())

  def tieOff: Unit = {
    if (directionOf(done) == ActualDirection.Output) {
      done := false.B
      running := false.B
      enable.ready := false.B
    } else
    {
      enable.valid := false.B
    }
  }
}

class ReactionPortIO(c: ReactionConfig) extends Bundle {
  val triggers = MixedVec(Seq.tabulate(c.triggers.length)(i => Flipped(new PortOutIO(c.triggers(i)))))
  val dependencies = MixedVec(Seq.tabulate(c.dependencies.length)(i => Flipped(new PortOutIO(c.dependencies(i)))))
  val antiDependencies = MixedVec(Seq.tabulate(c.antiDependencies.length)(i => Flipped(new PortInIO(c.antiDependencies(i)))))
}

abstract class Reaction(c: ReactionConfig) extends Module {
  val ioCtrl = IO(new ReactionCtrlIO())
  ioCtrl.tieOff
  val io = IO(new ReactionPortIO(c))
  io.triggers.map(_.reactionTieOff)
  io.dependencies.map(_.reactionTieOff)
  io.antiDependencies.map(_.reactionTieOff)

  // Reset signal to reset all Registers in the reactionBody
  val reactionEnable = Wire(Bool())
  reactionEnable := false.B
  val reactionDone = Wire(Bool())
  reactionDone := false.B

  def reactionBody: Unit

  val sIdle :: sRunning :: sDone :: Nil = Enum(3)
  val regStateTop = RegInit(sIdle)
  val regCycles = RegInit(0.U(16.W))


  // The reactionMain is the "mainLoop" of the Reaction. To avoid some quirks in Chisel'
  //  this is wrapped in a function and called from the child class
  def reactionMain: Unit = {
    switch(regStateTop) {

      is(sIdle) {
        ioCtrl.running := false.B
        ioCtrl.done := false.B
        regCycles := 0.U

        when(ioCtrl.enable.valid) {
          // TODO: Should check wether there are data in the port
          regStateTop := sRunning
          ioCtrl.enable.ready := true.B
        }
      }

      is(sRunning) {
        regCycles := regCycles + 1.U
        ioCtrl.running := true.B
        ioCtrl.done := false.B
        reactionEnable := true.B

        withReset(!reactionEnable) {
          reactionBody
        }

        when(reactionDone) {
          regStateTop := sDone
        }
      }

      is(sDone) {
        ioCtrl.done := true.B
        ioCtrl.running := true.B
        regStateTop := sIdle
      }
    }
  }

  assert(!(regCycles > 200.U), "[Reaction] Reaction was running for over 200cc assumed error")
}
// TODO: Handle the detection of reaction finished here

// TODO: What happpens if the Reaction is finished without consuming all the data in the reader queues?
abstract class ReactionStreaming(c: ReactionConfig) extends Reaction(c) {
  val triggerReader = Array.tabulate(c.triggers.length)(i => Module(new PortStreamReader(c.triggers(i))).io)
  val dependencyReader = Array.tabulate(c.dependencies.length)(i => Module(new PortStreamReader(c.dependencies(i))).io)
  val antiDependencyWriter = Array.tabulate(c.antiDependencies.length)(i => Module(new PortStreamWriter(c.antiDependencies(i))).io)
  triggerReader.foreach(_.tieOffExt())
  dependencyReader.foreach(_.tieOffExt())
  antiDependencyWriter.foreach(_.tieOffExt())

  // Connect the streaming readers and writers
  io.triggers zip triggerReader foreach {case (port, reader) => port <> reader.portRead}
  io.dependencies zip dependencyReader foreach {case (port, reader) => port <> reader.portRead}
  io.antiDependencies zip antiDependencyWriter foreach {case (port, writer) => port <> writer.portWrite}

  // Start the streaming readers when the reaction is enabled
  val streamIdle :: streamReading :: Nil = Enum(2)
  val regStreamState = RegInit(streamIdle)

  switch(regStreamState) {
    is (streamIdle) {
      when (regStateTop === sRunning) {
        // Start the streaming readers if there is data.
        for (rdr <- triggerReader ++ dependencyReader) {
          when(rdr.portRead.present) {
            rdr.start.valid := true.B
            assert(rdr.start.fire)
          }
        }
        regStreamState := streamReading
      }
    }

    is (streamReading) {
      when(reactionDone) {
        regStreamState := streamIdle
      }
    }
  }
}
