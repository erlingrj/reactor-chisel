package reactor.util

import chisel3._
import chisel3.util._
import reactor._
import fpgatidbits.synthutils.VivadoSynth
import fpgatidbits.synthutils.PrintableParam
import reactor.util.CharacterizeUtils.DummyParams

import Numeric._
import scala.collection.mutable.ArrayBuffer


object ReactorFault extends ChiselEnum {
  val None, ReactionTimeout, MemoryFault = Value
}

object CharacterizeUtils {
  class DummyParams extends PrintableParam {
    def headersAsList(): List[String] = List("")
    def contentAsList(): List[String] = List("")
  }
  val dummyParam = new DummyParams
  val zedboardPart = "xc7z020clg484-1"

  def standalone(reactor: () => StandaloneTopReactor, buildDir: String): Unit = {
    val genTop  = (d: DummyParams) => reactor.apply()
    VivadoSynth.characterizePoint(dummyParam, genTop, buildDir, zedboardPart,"StandaloneTopReactor")
  }

  def codesign(reactor: () => CodesignTopReactor, buildDir: String): Unit = {
    val genTop = (d: DummyParams) => reactor.apply()
    VivadoSynth.characterizePoint(dummyParam, genTop, buildDir, zedboardPart, "CodesignTopReactor")

  }

}