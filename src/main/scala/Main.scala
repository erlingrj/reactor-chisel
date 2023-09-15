package reactor

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._

import scala.sys.process.Process
import java.nio.file.{Files, Paths}
import fpgatidbits.PlatformWrapper.VerilatedTesterWrapper
import fpgatidbits.TidbitsMakeUtils._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.TidbitsMakeUtils

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
   def mainStandalone(mainReactorFunc: () => Reactor, targetDir: String, name: String, globalCfg: GlobalReactorConfig): Unit = {
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
   }

   def mainCodesign(platformInstFxn: PlatformInstFxn, mainReactorFunc: () => Reactor, mainReactorSwIOFunc: () => SwIO, targetDir: String, globalConfig: GlobalReactorConfig) = {
     println("------------------------------------------------------------------------------------------------------")
     println("Running Chisel compiler to generate verilator project")
     println("------------------------------------------------------------------------------------------------------")
     val chiselArgs = Array("--target-dir", s"$targetDir")
     (new chisel3.stage.ChiselStage).emitVerilog(platformInstFxn(p => new CodesignTopReactor(p, mainReactorFunc, mainReactorSwIOFunc)(globalConfig), targetDir), chiselArgs)
     if (platformInstFxn.equals(TidbitsMakeUtils.platformMap("VerilatedTester"))) {
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
 }