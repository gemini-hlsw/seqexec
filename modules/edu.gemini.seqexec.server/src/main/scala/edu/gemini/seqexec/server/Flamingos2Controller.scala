// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ImageFileId

import scala.concurrent.duration.Duration

import scalaz.Equal

trait Flamingos2Controller {
  import Flamingos2Controller._

  // I'm not sure if getConfig will be used. It made sense for TCS, because parts of the TCS configuration cannot be
  // inferred from the sequence, and because Seqexec needs to temporarily change parts of the TCS configuration only to
  // later revert those changes to the previous values. But for most (if not all) instruments, the sequence completely
  // defines the instrument configuration.
  def getConfig: SeqAction[Flamingos2Config]

  def applyConfig(config: Flamingos2Config): SeqAction[Unit]

  def observe(obsid: ImageFileId): SeqAction[ImageFileId]
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
    implicit val equal: Equal[FocalPlaneUnit] = Equal.equalA
  }

  type Filter = edu.gemini.spModel.gemini.flamingos2.Flamingos2.Filter

  type Lyot = edu.gemini.spModel.gemini.flamingos2.Flamingos2.LyotWheel

  sealed trait Grism
  object Grism {
    object Open extends Grism
    object R1200JH extends Grism
    object R1200HK extends Grism
    object R3000 extends Grism
    object Dark extends Grism
  }

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
    def setWindowCover(windowCover: WindowCover): CCConfig = this.copy(w = windowCover)
    def setDecker(decker: Decker): CCConfig = this.copy(d = decker)
    def setFPU(focalPlaneUnit: FocalPlaneUnit): CCConfig = this.copy(fpu = focalPlaneUnit)
    def setFilter(filter: Filter): CCConfig = this.copy(f = filter)
    def setLyot(lyot: Lyot): CCConfig = this.copy(l = lyot)
    def setGrism(grism: Grism): CCConfig = this.copy(g = grism)
  }

  final case class DCConfig(t: ExposureTime, n: Reads, r: ReadoutMode, b: BiasMode) {
    def setExposureTime(exposureTime: ExposureTime): DCConfig = this.copy(t = exposureTime)
    def setNumReads(numReads: Reads): DCConfig =  this.copy(n = numReads)
    def setReadoutMode(readoutMode: ReadoutMode): DCConfig = this.copy(r = readoutMode)
    def setBiasMode(biasMode: BiasMode): DCConfig = this.copy(b = biasMode)
  }

  final case class Flamingos2Config(cc: CCConfig, dc: DCConfig) {
    def setCCConfig(ccConfig: CCConfig): Flamingos2Config = this.copy(cc = ccConfig)
    def setDCConfig(dcConfig: DCConfig): Flamingos2Config = this.copy(dc = dcConfig)
  }

}
