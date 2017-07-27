package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.SeqexecFailure._
import squants.Length
import scala.concurrent.duration.Duration
import scalaz.\/
import edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI

trait GmosSouthController {
  import GmosSouthController._

  // I'm not sure if getConfig will be used. It made sense for TCS, because parts of the TCS configuration cannot be
  // inferred from the sequence, and because Seqexec needs to temporarily change parts of the TCS configuration only to
  // later revert those changes to the previous values. But for most (if not all) instruments, the sequence completely
  // defines the instrument configuration.
  def getConfig: SeqAction[GmosSouthConfig]

  def applyConfig(config: GmosSouthConfig): SeqAction[Unit]

  def observe(obsid: ImageFileId): SeqAction[ImageFileId]
}

object GmosSouthController {

  type Filter = edu.gemini.spModel.gemini.gmos.GmosSouthType.FilterSouth
  type FPU = edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth
  type GmosStageMode = edu.gemini.spModel.gemini.gmos.GmosSouthType.StageModeSouth
  type Disperser = edu.gemini.spModel.gemini.gmos.GmosSouthType.DisperserSouth
  type DTAX = edu.gemini.spModel.gemini.gmos.GmosCommonType.DTAX
  type ADC = edu.gemini.spModel.gemini.gmos.GmosCommonType.ADC
  type UseElectronicOffset = edu.gemini.spModel.gemini.gmos.InstGmosCommon.UseElectronicOffsettingRuling
  type DisperserOrder = edu.gemini.spModel.gemini.gmos.GmosCommonType.Order
  type Binning = edu.gemini.spModel.gemini.gmos.GmosCommonType.Binning
  type AmpReadMode = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpReadMode
  type AmpGain = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpGain
  type AmpCount = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpCount
  type BuiltinROI = edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
  type ROI = edu.gemini.spModel.gemini.gmos.GmosCommonType.ROIDescription
  type ExposureTime = Duration
  type PosAngle = edu.gemini.spModel.core.Angle

  // I'm not totally sure this is being used
  sealed trait BiasTime
  case object BiasTimeSet extends BiasTime
  case object BiasTimeEmpty extends BiasTime
  case object BiasTimeUnset extends BiasTime

  // Used for the shutterState
  sealed trait ShutterState
  case object UnsetShutter extends ShutterState
  case object OpenShutter extends ShutterState
  case object CloseShutter extends ShutterState

  sealed trait GmosFPU
  final case object UnknownFPU extends GmosFPU
  final case class BuiltInFPU(fpu: FPU) extends GmosFPU
  final case class CustomMaskFPU(mask: String) extends GmosFPU

  sealed trait Beam
  case object InBeam extends Beam
  case object OutOfBeam extends Beam

  final case class AmpGainSetting(value: Int)

  final case class GmosDisperser(disperser: Disperser, order: Option[DisperserOrder], lambda: Option[Length])

  final case class CCConfig(filter: Filter,
    disperser: GmosDisperser,
    fpu: GmosFPU,
    stage: GmosStageMode,
    dtaX: DTAX,
    adc: ADC,
    useElectronicOffset: Option[UseElectronicOffset])

  final case class CCDReadout(ampReadMode: AmpReadMode, ampGain: AmpGain, ampCount: AmpCount, gainSetting: Double)

  final case class CCDBinning(x: Binning, y: Binning)

  sealed abstract class RegionsOfInterest(val rois: BuiltinROI \/ List[ROI])
  object RegionsOfInterest {
    def fromOCS(builtIn: BuiltinROI, custom: List[ROI]): SeqexecFailure \/ RegionsOfInterest =
      (builtIn, custom) match {
        case (b, r) if b != BuiltinROI.CUSTOM && r.isEmpty => \/.right(new RegionsOfInterest(\/.left(b)) {})
        case (BuiltinROI.CUSTOM, r) if r.nonEmpty => \/.right(new RegionsOfInterest(\/.right(r)) {})
        case _ => \/.left(Unexpected("Inconsistent values for GMOS regions of interest"))
      }
    def unapply(r: RegionsOfInterest): Some[BuiltinROI \/ List[ROI]] = Some(r.rois)
  }

  final case class DCConfig(t: ExposureTime, b: BiasTime, s: ShutterState, r: CCDReadout, bi: CCDBinning, roi: RegionsOfInterest)

  final case class GmosSouthConfig(cc: CCConfig, dc: DCConfig)

}
