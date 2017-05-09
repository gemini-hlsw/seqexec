package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ImageFileId
import scala.concurrent.duration.Duration

trait GmosSController {
  import GmosSController._

  // I'm not sure if getConfig will be used. It made sense for TCS, because parts of the TCS configuration cannot be
  // inferred from the sequence, and because Seqexec needs to temporarily change parts of the TCS configuration only to
  // later revert those changes to the previous values. But for most (if not all) instruments, the sequence completely
  // defines the instrument configuration.
  def getConfig: SeqAction[GmosSConfig]

  def applyConfig(config: GmosSConfig): SeqAction[Unit]

  def observe(obsid: ImageFileId): SeqAction[ImageFileId]
}

object GmosSController {

  /*type WindowCover = edu.gemini.spModel.gemini.GmosS.GmosS.WindowCover

  type Decker = edu.gemini.spModel.gemini.GmosS.GmosS.Decker

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

  type Filter = edu.gemini.spModel.gemini.GmosS.GmosS.Filter

  type Lyot = edu.gemini.spModel.gemini.GmosS.GmosS.LyotWheel

  type Grism = edu.gemini.spModel.gemini.GmosS.GmosS.Disperser


  type Reads = edu.gemini.spModel.gemini.GmosS.GmosS.Reads

  type ReadoutMode = edu.gemini.spModel.gemini.GmosS.GmosS.ReadoutMode

  sealed trait BiasMode
  object BiasMode {
    object Imaging extends BiasMode
    object LongSlit extends BiasMode
    object MOS extends BiasMode
  }*/

  type Binning = edu.gemini.spModel.gemini.gmos.GmosCommonType.Binning
  type AmpReadMode = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpReadMode
  type AmpGain = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpGain
  type AmpCount = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpCount
  type ROI = edu.gemini.spModel.gemini.gmos.GmosCommonType.ROIDescription
  type ExposureTime = Duration

  final case class CCConfig()

  final case class CCDBinning(x: Binning, y: Binning)

  final case class CCDReadout(ampReadMode: AmpReadMode, gainChoice: AmpGain, ampCount: AmpCount, gainSetting: Double)

  final case class RegionsOfInterest(bulitInROI: ROI, customROI: List[ROI])

  final case class DCConfig(t: ExposureTime, r: CCDReadout, b: CCDBinning, roi: RegionsOfInterest)

  final case class GmosSConfig(cc: CCConfig, dc: DCConfig)

}
