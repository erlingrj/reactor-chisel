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

object ChiselMain {

  def main(args: Array[String]): Unit = {
    require(args.length > 1)
    val programType: String = args(0)

    programType match {
      case "standalone" => mainStandalone(args.drop(1))
      case "codesign" => mainCodesign(args.drop(1))
      case _ => require(false)
    }
  }
  def mainStandalone(args: Array[String]): Unit = {

    println(s"Running standalone generator with args")
    val example: String = args(0)
    val targetDir = if (args.length > 2) args(1) else s"build/$example"

    val mainReactorFunc = (example match {
      case "ReactorCounter" => Some(() => new ReactorCounter())
      case "ReactorCounterWithState" => Some(() => new ReactorCounterWithState())
      case _ => None
    }).get

    println(s"Compiling reactor-chisel with example `$example`")
    val chiselArgs = Array("--target-dir", s"$targetDir/tmp")
    implicit val globalCfg = GlobalReactorConfig(timeout = Time.NEVER)
    val verilog = (new chisel3.stage.ChiselStage).emitVerilog(new StandaloneMainReactor((mainReactorFunc)),chiselArgs)
    val saveLocation = targetDir + "/ReactorChisel.v"
    Settings.writeVerilogToFile(verilog, saveLocation)
    println(s"Wrote the generated verilog to `$saveLocation`")

    // Copy main cpp file for emulation
    fpgatidbits.TidbitsMakeUtils.fileCopy("src/main/resources/simulator/standalone/main.cpp", targetDir + "/main.cpp")
    fpgatidbits.TidbitsMakeUtils.fileCopy("src/main/resources/simulator/standalone/Makefile", targetDir + "/Makefile")
  }

  def mainCodesign(args: Array[String]): Unit = {
    require(args.length == 2)
    val example: String = args(0)
    val targetDir: String = args(1)

    val platformInst = {f: (PlatformWrapperParams => GenericAccelerator) => new VerilatedTesterWrapper(f, targetDir)}
    val accInst = (example match {
      case "TopReactorEx" => Some({p: PlatformWrapperParams => new TopReactorEx })
      case _ => None
    }).get

    val chiselArgs = Array("--target-dir", s"$targetDir/tmp")
    val verilogString = (new chisel3.stage.ChiselStage).emitVerilog(platformInst(accInst), chiselArgs)
    Settings.writeVerilogToFile(verilogString, targetDir + "/TesterWrapper.v")
    val resRoot = Paths.get("src/main/resources").toAbsolutePath.toString
    val tidbitsResRoot = Paths.get("fpga-tidbits/src/main/resources").toAbsolutePath.toString
    val resTestRoot = resRoot + "/" +  example
    fpgatidbits.TidbitsMakeUtils.fileCopy(tidbitsResRoot + "/script/VerilatorMakefile", targetDir + "/Makefile")
    fpgatidbits.TidbitsMakeUtils.fileCopy(resTestRoot + "/main.cpp", targetDir)
  }
}
