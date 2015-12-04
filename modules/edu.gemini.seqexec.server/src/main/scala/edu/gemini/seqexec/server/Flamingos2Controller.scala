package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.DhsClient.ObsId

/**
 * Created by jluhrs on 11/16/15.
 */
trait Flamingos2Controller {
  import Flamingos2Controller._

  def getConfig: SeqAction[Flamingos2Config]

  def applyConfig(config: Flamingos2Config): SeqAction[Unit]

  def observe(obsid: ObsId): SeqAction[ObsId]
}

object Flamingos2Controller {

//  sealed trait WindowCover
//  object WindowCover {
//    object Open extends WindowCover
//    object Closed extends WindowCover
//  }
  type WindowCover = edu.gemini.spModel.gemini.flamingos2.Flamingos2.WindowCover


//  sealed trait Decker
//  object Decker {
//    object Open extends Decker
//    object LongSlit extends Decker
//    object MOS extends Decker
//  }
  type Decker = edu.gemini.spModel.gemini.flamingos2.Flamingos2.Decker

  sealed trait FocalPlaneUnit
  object FocalPlaneUnit {
    object Open extends FocalPlaneUnit
    object GridSub1Pix extends FocalPlaneUnit
    object Grid2Pix extends FocalPlaneUnit
    object Slit1Pix extends FocalPlaneUnit
    object Slit2Pix extends FocalPlaneUnit
    object Slit3Pix extends FocalPlaneUnit
    object Slit4Pix extends FocalPlaneUnit
    object Slit6Pix extends FocalPlaneUnit
    object Slit8Pix extends FocalPlaneUnit
    final case class Custom(mask: String) extends FocalPlaneUnit
  }

//  sealed case class Filter(str: String)
//  object Filter {
//    object Open extends Filter("Open")
//    object Dark extends Filter("DK_G0807")
//    object JLow extends Filter("J-lo_G0801")
//    object J extends Filter("J_G0802")
//    object H extends Filter("H_G0803")
//    object KLong extends Filter("K-long_G0812")
//    object Ks extends Filter("Ks_G0804")
//    object JH extends Filter("JH_G0809")
//    object HK extends Filter("HK_G0806_good")
//    object Y extends Filter("Y_G0811")
//    object F1056 extends Filter("F1056")
//    object F1063 extends Filter("F1063")
//  }
  type Filter = edu.gemini.spModel.gemini.flamingos2.Flamingos2.Filter

//  sealed case class Lyot(str: String)
//  object Lyot {
//    object F16 extends Lyot("f/16_G5830")
//    object GEMSUnder extends Lyot("GEMS_under_G5835")
//    object GEMSOver extends Lyot("GEMS_over_G5836")
//    object Hartmann1 extends Lyot("Hart1_G5833")
//    object Hartmann2 extends Lyot("Hart2_G5834")
//  }
  type Lyot = edu.gemini.spModel.gemini.flamingos2.Flamingos2.LyotWheel


//  sealed case class Grism(str: String)
//  object Grism {
//    object Open extends Grism("Open")
//    object Dark extends Grism("DK_G5804")
//    object JH extends Grism("JH_G5801")
//    object HK extends Grism("HK_G5802")
//    object R3K extends Grism("R3K_G5803")
//  }
  type Grism = edu.gemini.spModel.gemini.flamingos2.Flamingos2.Disperser

  type ExposureTime = squants.Time

  type Reads = edu.gemini.spModel.gemini.flamingos2.Flamingos2.Reads

//  sealed case class ReadoutMode(str: String)
//  object ReadoutMode {
//    object Science extends ReadoutMode("SCI")
//    object Engineering extends ReadoutMode("ENG")
//  }
  type ReadoutMode = edu.gemini.spModel.gemini.flamingos2.Flamingos2.ReadoutMode

  sealed trait BiasMode
  object BiasMode {
    object Imaging extends BiasMode
    object LongSlit extends BiasMode
    object MOS extends BiasMode
  }

  final case class CCConfig(w: WindowCover, d: Decker, fpu: FocalPlaneUnit, f: Filter, l: Lyot, g: Grism) {
    def setWindowCover(windowCover: WindowCover) = this.copy(w = windowCover)
    def setDecker(decker: Decker) = this.copy(d = decker)
    def setFPU(focalPlaneUnit: FocalPlaneUnit) = this.copy(fpu = focalPlaneUnit)
    def setFilter(filter: Filter) = this.copy(f = filter)
    def setLyot(lyot: Lyot) = this.copy(l = lyot)
    def setGrism(grism: Grism) = this.copy(g = grism)
  }
  final case class DCConfig(t: ExposureTime, n: Reads, r: ReadoutMode, b: BiasMode) {
    def setExposureTime(exposureTime: ExposureTime) = this.copy(t = exposureTime)
    def setNumReads(numReads: Reads) =  this.copy(n = numReads)
    def setReadoutMode(readoutMode: ReadoutMode) = this.copy(r = readoutMode)
    def setBiasMode(biasMode: BiasMode) = this.copy(b = biasMode)
  }
  final case class Flamingos2Config(cc: CCConfig, dc: DCConfig) {
    def setCCConfig(ccConfig: CCConfig) = this.copy(cc = ccConfig)
    def setDCConfig(dcConfig: DCConfig) = this.copy(dc = dcConfig)
  }

}
