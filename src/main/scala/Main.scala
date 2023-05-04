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
    require(args.length == 2)
    val example: String = args(0)
    val targetDir: String = args(1)

    val platformInst = {f: (PlatformWrapperParams => GenericAccelerator) => new VerilatedTesterWrapper(f, targetDir)}
    val accInst = (example match {
      case "TopReactorEx" => Some({p: PlatformWrapperParams => new TopReactorEx })
      case _ => None
    }).get

    val verilogString = (new chisel3.stage.ChiselStage).emitVerilog(platformInst(accInst))
    Settings.writeVerilogToFile(verilogString, targetDir + "/TesterWrapper.v")
    val resRoot = Paths.get("src/main/resources").toAbsolutePath.toString
    val tidbitsResRoot = Paths.get("fpga-tidbits/src/main/resources").toAbsolutePath.toString
    val resTestRoot = resRoot + "/" +  example
    fpgatidbits.TidbitsMakeUtils.fileCopy(tidbitsResRoot + "/script/VerilatorMakefile", targetDir + "/Makefile")
    fpgatidbits.TidbitsMakeUtils.fileCopy(resTestRoot + "/main.cpp", targetDir)
  }
}
