package reactor
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

object ChiselMain {
  def main(args: Array[String]): Unit = {
    val targetDir: String = args(0)

    val platformInst = {f: (PlatformWrapperParams => GenericAccelerator) => new VerilatedTesterWrapper(f, targetDir)}
    val accInst = {p: PlatformWrapperParams => new VaddStreamReactor(p,5) }

    val verilogString = (new chisel3.stage.ChiselStage).emitVerilog(platformInst(accInst))
    Settings.writeVerilogToFile(verilogString, targetDir + "/TesterWrapper.v")
    val resRoot = Paths.get("src/main/resources").toAbsolutePath.toString
    val resTestRoot = resRoot + "/TestVaddReactor"
    fpgatidbits.TidbitsMakeUtils.fileCopy(resRoot + "/Makefile", targetDir)
    fpgatidbits.TidbitsMakeUtils.fileCopy(resTestRoot + "/main.cpp", targetDir)
  }
}
