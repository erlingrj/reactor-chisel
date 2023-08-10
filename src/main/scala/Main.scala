package reactor

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._

import reactor.examples._
import fpgatidbits.PlatformWrapper._

import java.nio.file.Paths

object Settings {
  def writeVerilogToFile(verilog: String, path: String) = {
    import java.io._
    val fname = path
    val f = new File(fname)
    if (!f.exists()) {
      f.getParentFile.mkdirs
      f.createNewFile()
    }
    val writer = new PrintWriter(f)
    writer.write(verilog)
    writer.close()
  }
}