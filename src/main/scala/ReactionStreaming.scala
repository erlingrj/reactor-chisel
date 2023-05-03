//package reactor
//
//import chisel3._
//import chisel3.util._
//
//// TODO: What happpens if the Reaction is finished without consuming all the data in the reader queues?
//abstract class ReactionStreaming(c: ReactionConfig) extends Reaction(c) {
//  val triggerReader = Array.tabulate(c.triggers.length)(i => Module(new PortStreamReader(c.triggers(i))).io)
//  val dependencyReader = Array.tabulate(c.dependencies.length)(i => Module(new PortStreamReader(c.dependencies(i))).io)
//  val antiDependencyWriter = Array.tabulate(c.antiDependencies.length)(i => Module(new PortStreamWriter(c.antiDependencies(i))).io)
//  triggerReader.foreach(_.tieOffExt())
//  dependencyReader.foreach(_.tieOffExt())
//  antiDependencyWriter.foreach(_.tieOffExt())
//
//  // Connect the streaming readers and writers
//  io.triggers zip triggerReader foreach {case (port, reader) => port <> reader.portRead}
//  io.dependencies zip dependencyReader foreach {case (port, reader) => port <> reader.portRead}
//  io.antiDependencies zip antiDependencyWriter foreach {case (port, writer) => port <> writer.portWrite}
//
//  // Start the streaming readers when the reaction is enabled
//  val streamIdle :: streamReading :: Nil = Enum(2)
//  val regStreamState = RegInit(streamIdle)
//
//  switch(regStreamState) {
//    is (streamIdle) {
//      when (regStateTop === sRunning) {
//        // Start the streaming readers if there is data.
//        for (rdr <- triggerReader ++ dependencyReader) {
//          when(rdr.portRead.present) {
//            rdr.start.valid := true.B
//            assert(rdr.start.fire)
//          }
//        }
//        regStreamState := streamReading
//      }
//    }
//
//    is (streamReading) {
//      when(reactionDone) {
//        regStreamState := streamIdle
//      }
//    }
//  }
//}
