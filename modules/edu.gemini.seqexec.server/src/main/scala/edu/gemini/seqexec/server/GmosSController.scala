package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ImageFileId
import squants.Length
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

  type Filter = edu.gemini.spModel.gemini.gmos.GmosSouthType.FilterSouth
  type FPUSouth = edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth
  type GmosSouthStageMode = edu.gemini.spModel.gemini.gmos.GmosSouthType.StageModeSouth
  type Disperser = edu.gemini.spModel.gemini.gmos.GmosSouthType.DisperserSouth
  type DTAX = edu.gemini.spModel.gemini.gmos.GmosCommonType.DTAX
  type ADC = edu.gemini.spModel.gemini.gmos.GmosCommonType.ADC
  type UseElectronicOffset = edu.gemini.spModel.gemini.gmos.InstGmosCommon.UseElectronicOffsettingRuling
  type DisperserOrder = edu.gemini.spModel.gemini.gmos.GmosCommonType.Order
  type Binning = edu.gemini.spModel.gemini.gmos.GmosCommonType.Binning
  type AmpReadMode = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpReadMode
  type AmpGain = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpGain
  type AmpCount = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpCount
  type ROI = edu.gemini.spModel.gemini.gmos.GmosCommonType.ROIDescription
  type ExposureTime = Duration
  type PosAngle = edu.gemini.spModel.core.Angle

  sealed trait GmosSouthFPU
  final case class BuiltInFPU(fpu: FPUSouth) extends GmosSouthFPU
  final case class CustomFPU(mask: String) extends GmosSouthFPU

  final case class GmosSouthDisperser(disperser: Disperser, order: Option[Disperser], lambda: Option[Length])

  final case class CCConfig(posAngle: PosAngle,
    filter: Filter,
    disperser: GmosSouthDisperser,
    fpu: GmosSouthFPU,
    stage: GmosSouthStageMode,
    dtaX: DTAX,
    adc: ADC,
    useElectronicOffset: Option[UseElectronicOffset])

  final case class CCDBinning(x: Binning, y: Binning)

  final case class CCDReadout(ampReadMode: AmpReadMode, gainChoice: AmpGain, ampCount: AmpCount, gainSetting: Double)

  final case class RegionsOfInterest(bulitInROI: ROI, customROI: List[ROI])

  final case class DCConfig(t: ExposureTime, r: CCDReadout, b: CCDBinning, roi: RegionsOfInterest)

  final case class GmosSConfig(cc: CCConfig, dc: DCConfig)

}
