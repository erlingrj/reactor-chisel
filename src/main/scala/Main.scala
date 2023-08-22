package reactor

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import reactor.examples._
import scala.sys.process.Process
import java.nio.file.{Files, Paths}
import fpgatidbits.PlatformWrapper.VerilatedTesterWrapper
import fpgatidbits.TidbitsMakeUtils._
import fpgatidbits.PlatformWrapper._

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
 object ReactorChisel {
   def mainStandalone(mainReactorFunc: () => Reactor, targetDir: String, binDir: String, name: String, globalCfg: GlobalReactorConfig): Unit = {
     println("------------------------------------------------------------------------------------------------------")
     println("Running Chisel compiler to generate verilator project")
     println("------------------------------------------------------------------------------------------------------")
     val chiselArgs = Array("--target-dir", s"$targetDir/tmp")
     val verilog = (new chisel3.stage.ChiselStage).emitVerilog(new StandaloneTopReactor((mainReactorFunc))(globalCfg), chiselArgs)
     val saveLocation = targetDir + "/ReactorChisel.v"
     Settings.writeVerilogToFile(verilog, saveLocation)
     println(s"Wrote the generated verilog to `$saveLocation`")
     // Copy main cpp file for emulation
     resourceCopyBulk("simulator/standalone", targetDir, Seq("main.cpp", "Makefile"))
     println("------------------------------------------------------------------------------------------------------")
     println("Building verilator project")
     println("------------------------------------------------------------------------------------------------------")
     val result = Process("make", new java.io.File(targetDir)).!
     println("------------------------------------------------------------------------------------------------------")
     println("Copying executable into bin dir. ")
     println("------------------------------------------------------------------------------------------------------")
     if (!Files.exists(Paths.get(binDir))) {
       Files.createDirectories(Paths.get(binDir))
     }
     fpgatidbits.TidbitsMakeUtils.fileCopy(s"$targetDir/emu", s"$binDir/$name")
     println("------------------------------------------------------------------------------------------------------")
     println(s"To execute program run:`$binDir/$name`")
     println("------------------------------------------------------------------------------------------------------")
   }

   def mainCodesign(mainReactorFunc: () => Reactor, mainReactorSwIOFunc: () => SwIO, targetDir: String, globalConfig: GlobalReactorConfig) = {
     println("------------------------------------------------------------------------------------------------------")
     println("Running Chisel compiler to generate verilator project")
     println("------------------------------------------------------------------------------------------------------")
     val chiselArgs = Array("--target-dir", s"$targetDir")
     (new chisel3.stage.ChiselStage).emitVerilog(new VerilatedTesterWrapper(_ => new CodesignTopReactor(mainReactorFunc, mainReactorSwIOFunc)(globalConfig), targetDir), chiselArgs)
     // Copy main cpp file for emulation
     resourceCopyBulk("simulator/codesign", targetDir, Seq("main.cpp", "Makefile"))
     println("------------------------------------------------------------------------------------------------------")
     println("Building verilator project")
     println("------------------------------------------------------------------------------------------------------")
     val result = Process("make lib", new java.io.File(targetDir)).!
     println("------------------------------------------------------------------------------------------------------")
     println(s"Library produced to $targetDir/lfFPGA.a.")
     println("------------------------------------------------------------------------------------------------------")
   }
 }