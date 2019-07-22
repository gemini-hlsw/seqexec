// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.Eq
import cats.Show
import cats.implicits._
import edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
import edu.gemini.spModel.gemini.gmos.{GmosNorthType, GmosSouthType}
import gsp.math.Offset
import scala.concurrent.duration.Duration
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Guiding
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.gmos.GmosController.Config.DCConfig
import seqexec.server.gmos.GmosController.Config.NSConfig
import seqexec.server.SeqexecFailure.Unexpected
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server._
import squants.{Length, Time}

trait GmosController[F[_], T <: GmosController.SiteDependentTypes] {
  import GmosController._

  def applyConfig(config: GmosConfig[T]): F[Unit]

  def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult]

  // endObserve is to notify the completion of the observation, not to cause its end.
  def endObserve: F[Unit]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def pauseObserve: F[Unit]

  def resumePaused(expTime: Time): F[ObserveCommandResult]

  def stopPaused: F[ObserveCommandResult]

  def abortPaused: F[ObserveCommandResult]

  def observeProgress(total: Time, elapsed: ElapsedTime): fs2.Stream[F, Progress]

}

object GmosController {

  sealed abstract class Config[T<:SiteDependentTypes] {
    import Config._

    case class BuiltInFPU(fpu: T#FPU) extends GmosFPU

    sealed trait GmosDisperser
    object GmosDisperser {
      case object Mirror extends GmosDisperser
      case class Order0(disperser: T#Disperser) extends GmosDisperser
      case class OrderN(disperser: T#Disperser, order: DisperserOrder, lambda: Length)
        extends GmosDisperser
    }

    val mirror: T#Disperser
    def isMirror(v: T#Disperser): Boolean

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
    sealed trait BiasTime extends Product with Serializable

    object BiasTime {
      case object BiasTimeSet extends BiasTime
      case object BiasTimeEmpty extends BiasTime
      case object BiasTimeUnset extends BiasTime
    }

    // Used for the shutterState
    sealed trait ShutterState extends Product with Serializable

    object ShutterState {
      case object UnsetShutter extends ShutterState
      case object OpenShutter extends ShutterState
      case object CloseShutter extends ShutterState
    }

    sealed trait Beam extends Product with Serializable

    object Beam {
      case object InBeam extends Beam
      case object OutOfBeam extends Beam
    }

    sealed trait GmosFPU

    final case object UnknownFPU extends GmosFPU

    final case class CustomMaskFPU(mask: String) extends GmosFPU

    final case class CCDReadout(ampReadMode: AmpReadMode, ampGain: AmpGain, ampCount: AmpCount, gainSetting: Double)

    final case class CCDBinning(x: Binning, y: Binning)

    sealed abstract class RegionsOfInterest(val rois: Either[BuiltinROI, List[ROI]])

    // Node and shuffle positions
    final case class NSPosition(id: Symbol, offset: Offset, guide: Guiding)

    // Node and shuffle options
    sealed trait NSConfig extends Product with Serializable {
      def nsPairs: Int
    }

    object NSConfig {
      case object NoNodAndShuffle extends NSConfig {
        val nsPairs = 0
      }
      final case class NodAndShuffle(cycles: Int, rows: Int, positions: Vector[NSPosition]) extends NSConfig {
        val nsPairs = cycles
      }
    }

    object RegionsOfInterest {
      def fromOCS(builtIn: BuiltinROI, custom: List[ROI]): Either[SeqexecFailure, RegionsOfInterest] =
        (builtIn, custom) match {
          case (b, r) if b =!= BuiltinROI.CUSTOM && r.isEmpty => new RegionsOfInterest(b.asLeft) {}.asRight
          case (BuiltinROI.CUSTOM, r) if r.nonEmpty           => new RegionsOfInterest(r.asRight) {}.asRight
          case _                                              => Unexpected("Inconsistent values for GMOS regions of interest").asLeft
        }

      def unapply(r: RegionsOfInterest): Option[Either[BuiltinROI, List[ROI]]] = r.rois.some
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

  sealed trait SiteDependentTypes {
    type Filter
    type FPU
    type GmosStageMode
    type Disperser

    val disperserEq: Eq[Disperser]
  }

  final class SouthTypes extends SiteDependentTypes {
    override type Filter        = edu.gemini.spModel.gemini.gmos.GmosSouthType.FilterSouth
    override type FPU           = edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth
    override type GmosStageMode = edu.gemini.spModel.gemini.gmos.GmosSouthType.StageModeSouth
    override type Disperser     = edu.gemini.spModel.gemini.gmos.GmosSouthType.DisperserSouth

    override val disperserEq: Eq[GmosSouthType.DisperserSouth] = Eq.fromUniversalEquals
  }

  final class SouthConfigTypes extends Config[SouthTypes] {
    override val mirror = edu.gemini.spModel.gemini.gmos.GmosSouthType.DisperserSouth.MIRROR
    override def isMirror(v: GmosSouthType.DisperserSouth): Boolean = v === mirror
  }
  val southConfigTypes: SouthConfigTypes = new SouthConfigTypes

  final class NorthTypes extends SiteDependentTypes {
    override type Filter        = edu.gemini.spModel.gemini.gmos.GmosNorthType.FilterNorth
    override type FPU           = edu.gemini.spModel.gemini.gmos.GmosNorthType.FPUnitNorth
    override type GmosStageMode = edu.gemini.spModel.gemini.gmos.GmosNorthType.StageModeNorth
    override type Disperser     = edu.gemini.spModel.gemini.gmos.GmosNorthType.DisperserNorth

    override val disperserEq: Eq[GmosNorthType.DisperserNorth] = Eq.fromUniversalEquals
  }

  final class NorthConfigTypes extends Config[NorthTypes] {
    override val mirror = edu.gemini.spModel.gemini.gmos.GmosNorthType.DisperserNorth.MIRROR
    override def isMirror(v: GmosNorthType.DisperserNorth): Boolean = v === mirror
  }
  val northConfigTypes: NorthConfigTypes = new NorthConfigTypes

  // This is a trick to allow using a type from a class parameter to define the type of another class parameter
  class GmosConfig[T<:SiteDependentTypes] private (val cc: Config[T]#CCConfig, val dc: DCConfig, val c: Config[T], val ns: NSConfig) {
    def this(c: Config[T])(cc: c.CCConfig, dc: DCConfig, ns: NSConfig) = this(cc, dc, c, ns)
  }

  type GmosSouthController[F[_]] = GmosController[F, SouthTypes]

  type GmosNorthController[F[_]] = GmosController[F, NorthTypes]

  implicit def configShow[T<:SiteDependentTypes]: Show[GmosConfig[T]] =
    Show.show { config => s"(${config.cc.filter}, ${config.cc.disperser}, ${config.cc.fpu}, ${config.cc.stage}, ${config.cc.stage}, ${config.cc.dtaX}, ${config.cc.adc}, ${config.cc.useElectronicOffset}, ${config.dc.t}, ${config.dc.b}, ${config.dc.s}, ${config.dc.bi}, ${config.dc.roi.rois})" }

}
