// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import seqexec.model.dhs.ImageFileId
import seqexec.server.gmos.GmosController.Config.DCConfig
import seqexec.server.SeqexecFailure.Unexpected
import seqexec.server._
import edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
import squants.{Length, Time}

import scala.concurrent.duration.Duration
import cats.implicits._

trait GmosController[T<:GmosController.SiteDependentTypes] {
  import GmosController._

  // I'm not sure if getConfig will be used. It made sense for TCS, because parts of the TCS configuration cannot be
  // inferred from the sequence, and because Seqexec needs to temporarily change parts of the TCS configuration only to
  // later revert those changes to the previous values. But for most (if not all) instruments, the sequence completely
  // defines the instrument configuration.
  def getConfig: SeqAction[GmosConfig[T]]

  def applyConfig(config: GmosConfig[T]): SeqAction[Unit]

  def observe(fileId: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result]

  // endObserve is to notify the completion of the observation, not to cause its end.
  def endObserve: SeqAction[Unit]

  def stopObserve: SeqAction[Unit]

  def abortObserve: SeqAction[Unit]

  def pauseObserve: SeqAction[Unit]

  def resumePaused(expTime: Time): SeqAction[ObserveCommand.Result]

  def stopPaused: SeqAction[ObserveCommand.Result]

  def abortPaused: SeqAction[ObserveCommand.Result]

}

object GmosController {

  final class Config[T<:SiteDependentTypes] {
    import Config._

    @SuppressWarnings(Array("org.wartremover.warts.LeakingSealed"))
    case class BuiltInFPU(fpu: T#FPU) extends GmosFPU

    case class GmosDisperser(disperser: T#Disperser, order: Option[DisperserOrder], lambda: Option[Length])

    case class CCConfig(
      filter: T#Filter,
      disperser: GmosDisperser,
      fpu: GmosFPU,
      stage: T#GmosStageMode,
      dtaX: DTAX,
      adc: ADC,
      useElectronicOffset: Option[UseElectronicOffset]
    )

  }

  // scalastyle:off
  object Config {
    type DTAX                = edu.gemini.spModel.gemini.gmos.GmosCommonType.DTAX
    type ADC                 = edu.gemini.spModel.gemini.gmos.GmosCommonType.ADC
    type UseElectronicOffset = edu.gemini.spModel.gemini.gmos.InstGmosCommon.UseElectronicOffsettingRuling
    type DisperserOrder      = edu.gemini.spModel.gemini.gmos.GmosCommonType.Order
    type Binning             = edu.gemini.spModel.gemini.gmos.GmosCommonType.Binning
    type AmpReadMode         = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpReadMode
    type AmpGain             = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpGain
    type AmpCount            = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpCount
    type BuiltinROI          = edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
    type ROI                 = edu.gemini.spModel.gemini.gmos.GmosCommonType.ROIDescription
    type ExposureTime        = Duration
    type PosAngle            = edu.gemini.spModel.core.Angle

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

    sealed trait Beam

    case object InBeam extends Beam

    case object OutOfBeam extends Beam

    sealed trait GmosFPU

    final case object UnknownFPU extends GmosFPU

    final case class CustomMaskFPU(mask: String) extends GmosFPU

    final case class CCDReadout(ampReadMode: AmpReadMode, ampGain: AmpGain, ampCount: AmpCount, gainSetting: Double)

    final case class CCDBinning(x: Binning, y: Binning)

    sealed abstract class RegionsOfInterest(val rois: Either[BuiltinROI, List[ROI]])

    object RegionsOfInterest {
      def fromOCS(builtIn: BuiltinROI, custom: List[ROI]): Either[SeqexecFailure, RegionsOfInterest] =
        (builtIn, custom) match {
          case (b, r) if b =!= BuiltinROI.CUSTOM && r.isEmpty => new RegionsOfInterest(b.asLeft) {}.asRight
          case (BuiltinROI.CUSTOM, r) if r.nonEmpty           => new RegionsOfInterest(r.asRight) {}.asRight
          case _                                              => Unexpected("Inconsistent values for GMOS regions of interest").asLeft
        }

      def unapply(r: RegionsOfInterest): Some[Either[BuiltinROI, List[ROI]]] = Some(r.rois)
    }

    final case class DCConfig(
      t: ExposureTime,
      b: BiasTime,
      s: ShutterState,
      r: CCDReadout,
      bi: CCDBinning,
      roi: RegionsOfInterest
    )

  }
  // scalastyle:on

  sealed trait SiteDependentTypes {
    type Filter
    type FPU
    type GmosStageMode
    type Disperser
  }

  final class SouthTypes extends SiteDependentTypes {
    override type Filter        = edu.gemini.spModel.gemini.gmos.GmosSouthType.FilterSouth
    override type FPU           = edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth
    override type GmosStageMode = edu.gemini.spModel.gemini.gmos.GmosSouthType.StageModeSouth
    override type Disperser     = edu.gemini.spModel.gemini.gmos.GmosSouthType.DisperserSouth
  }

  type SouthConfigTypes = Config[SouthTypes]
  val southConfigTypes: SouthConfigTypes = new SouthConfigTypes

  final class NorthTypes extends SiteDependentTypes {
    override type Filter        = edu.gemini.spModel.gemini.gmos.GmosNorthType.FilterNorth
    override type FPU           = edu.gemini.spModel.gemini.gmos.GmosNorthType.FPUnitNorth
    override type GmosStageMode = edu.gemini.spModel.gemini.gmos.GmosNorthType.StageModeNorth
    override type Disperser     = edu.gemini.spModel.gemini.gmos.GmosNorthType.DisperserNorth
  }

  type NorthConfigTypes = Config[NorthTypes]
  val northConfigTypes: NorthConfigTypes = new NorthConfigTypes

  // This is a trick to allow using a type from a class parameter to define the type of another class parameter
  class GmosConfig[T<:SiteDependentTypes] private (val cc: Config[T]#CCConfig, val dc: DCConfig, val c: Config[T]) {
    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def this(c: Config[T])(cc: c.CCConfig, dc: DCConfig) = this(cc, dc, c)
  }

  type GmosSouthController = GmosController[SouthTypes]

  type GmosNorthController = GmosController[NorthTypes]

}
