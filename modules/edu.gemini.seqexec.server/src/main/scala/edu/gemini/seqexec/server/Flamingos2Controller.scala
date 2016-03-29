package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.DhsClient.ObsId

import scala.concurrent.duration.Duration

/**
 * Created by jluhrs on 11/16/15.
 */
trait Flamingos2Controller {
  import Flamingos2Controller._

  // I'm not sure if getConfig will be used. It made sense for TCS, because parts of the TCS configuration cannot be
  // inferred from the sequence, and because Seqexec needs to temporarily change parts of the TCS configuration only to
  // later revert those changes to the previous values. But for most (if not all) instruments, the sequence completely
  // defines the instrument configuration.
  def getConfig: SeqAction[Flamingos2Config]

  def applyConfig(config: Flamingos2Config): SeqAction[Unit]

  def observe(obsid: ObsId): SeqAction[ObsId]
}

object Flamingos2Controller {

  type WindowCover = edu.gemini.spModel.gemini.flamingos2.Flamingos2.WindowCover

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

  type Filter = edu.gemini.spModel.gemini.flamingos2.Flamingos2.Filter

  type Lyot = edu.gemini.spModel.gemini.flamingos2.Flamingos2.LyotWheel

  type Grism = edu.gemini.spModel.gemini.flamingos2.Flamingos2.Disperser

  type ExposureTime = Duration

  type Reads = edu.gemini.spModel.gemini.flamingos2.Flamingos2.Reads

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
