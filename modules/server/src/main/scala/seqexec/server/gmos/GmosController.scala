// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import scala.concurrent.duration.Duration

import cats.Show
import cats.syntax.all._
import edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
import edu.gemini.spModel.gemini.gmos.GmosNorthType
import edu.gemini.spModel.gemini.gmos.GmosSouthType
import lucuma.core.math.Offset
import lucuma.core.util.Enumerated
import seqexec.model.GmosParameters._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Guiding
import seqexec.model.enum.NodAndShuffleStage
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.SeqexecFailure.Unexpected
import seqexec.server._
import seqexec.server.gmos.GmosController.Config.DCConfig
import seqexec.server.gmos.GmosController.Config.NSConfig
import shapeless.tag
import squants.Length
import squants.Time

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

  def nsCount: F[Int]

}

object GmosController {
  sealed abstract class Config[T <: SiteDependentTypes] {
    import Config._

    case class BuiltInFPU(fpu: T#FPU) extends GmosFPU

    sealed trait GmosDisperser extends Product with Serializable
    object GmosDisperser {
      case object Mirror                        extends GmosDisperser
      case class Order0(disperser: T#Disperser) extends GmosDisperser
      case class OrderN(disperser: T#Disperser, order: DisperserOrder, lambda: Length)
          extends GmosDisperser
    }

    val mirror: T#Disperser
    def isMirror(v: T#Disperser): Boolean

    case class CCConfig(
      filter:              T#Filter,
      disperser:           GmosDisperser,
      fpu:                 GmosFPU,
      stage:               T#GmosStageMode,
      dtaX:                DTAX,
      adc:                 ADC,
      useElectronicOffset: ElectronicOffset,
      isDarkOrBias:        Boolean
    )

  }

  object Config {
    type DTAX           = edu.gemini.spModel.gemini.gmos.GmosCommonType.DTAX
    type ADC            = edu.gemini.spModel.gemini.gmos.GmosCommonType.ADC
    type DisperserOrder = edu.gemini.spModel.gemini.gmos.GmosCommonType.Order
    type Binning        = edu.gemini.spModel.gemini.gmos.GmosCommonType.Binning
    type AmpReadMode    = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpReadMode
    type AmpGain        = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpGain
    type AmpCount       = edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpCount
    type BuiltinROI     = edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
    type ROI            = edu.gemini.spModel.gemini.gmos.GmosCommonType.ROIDescription
    type ExposureTime   = Duration
    type PosAngle       = edu.gemini.spModel.core.Angle

    // Used for the shutterState
    sealed trait ShutterState extends Product with Serializable

    object ShutterState {
      case object UnsetShutter extends ShutterState
      case object OpenShutter  extends ShutterState
      case object CloseShutter extends ShutterState

      /** @group Typeclass Instances */
      implicit val ShutterStateEnumerated: Enumerated[ShutterState] =
        Enumerated.of(UnsetShutter, OpenShutter, CloseShutter)
    }

    sealed trait Beam extends Product with Serializable

    object Beam {
      case object InBeam    extends Beam
      case object OutOfBeam extends Beam

      /** @group Typeclass Instances */
      implicit val BeamEnumerated: Enumerated[Beam] =
        Enumerated.of(InBeam, OutOfBeam)
    }

    sealed trait ElectronicOffset extends Product with Serializable

    object ElectronicOffset {
      case object On  extends ElectronicOffset
      case object Off extends ElectronicOffset

      def fromBoolean(v: Boolean): ElectronicOffset =
        if (v) On else Off

      /** @group Typeclass Instances */
      implicit val ElectronicOffsetEnumerated: Enumerated[ElectronicOffset] =
        Enumerated.of(On, Off)
    }

    sealed trait GmosFPU extends Product with Serializable

    final case object UnknownFPU extends GmosFPU

    final case class CustomMaskFPU(mask: String) extends GmosFPU

    final case class CCDReadout(
      ampReadMode: AmpReadMode,
      ampGain:     AmpGain,
      ampCount:    AmpCount,
      gainSetting: Double
    )

    final case class CCDBinning(x: Binning, y: Binning)

    sealed abstract class RegionsOfInterest(val rois: Either[BuiltinROI, List[ROI]])

    // Node and shuffle positions
    final case class NSPosition(stage: NodAndShuffleStage, offset: Offset, guide: Guiding)

    // Node and shuffle options
    sealed trait NSConfig extends Product with Serializable {
      def nsPairs: NsPairs
      def nsRows: NsRows
      def exposureDivider: NsExposureDivider
      def nsState: NodAndShuffleState
    }

    object NSConfig {
      case object NoNodAndShuffle extends NSConfig {
        val nsPairs         = tag[NsPairsI][Int](0)
        val nsRows          = tag[NsRowsI][Int](0)
        val exposureDivider = tag[NsExposureDividerI][Int](1)
        val nsState         = NodAndShuffleState.Classic
      }

      final case class NodAndShuffle(
        cycles:       NsCycles,
        rows:         NsRows,
        positions:    Vector[NSPosition],
        exposureTime: Time
      ) extends NSConfig {
        val nsPairs                 = tag[NsPairsI][Int](cycles * NodAndShuffleStage.NsSequence.length / 2)
        val nsRows                  = tag[NsRowsI][Int](Gmos.rowsToShuffle(NodAndShuffleStage.NsSequence.head, rows))
        val exposureDivider         = tag[NsExposureDividerI][Int](2)
        val nsState                 = NodAndShuffleState.NodShuffle
        val totalExposureTime: Time =
          cycles * exposureTime / exposureDivider.toDouble
        val nodExposureTime: Time   =
          exposureTime / exposureDivider.toDouble
      }
    }

    object RegionsOfInterest {
      def fromOCS(
        builtIn: BuiltinROI,
        custom:  List[ROI]
      ): Either[SeqexecFailure, RegionsOfInterest] =
        (builtIn, custom) match {
          case (b, r) if b =!= BuiltinROI.CUSTOM && r.isEmpty =>
            new RegionsOfInterest(b.asLeft) {}.asRight
          case (BuiltinROI.CUSTOM, r) if r.nonEmpty           => new RegionsOfInterest(r.asRight) {}.asRight
          case _                                              => Unexpected("Inconsistent values for GMOS regions of interest").asLeft
        }

      def unapply(r: RegionsOfInterest): Option[Either[BuiltinROI, List[ROI]]] = r.rois.some
    }

    final case class DCConfig(
      t:   ExposureTime,
      s:   ShutterState,
      r:   CCDReadout,
      bi:  CCDBinning,
      roi: RegionsOfInterest
    )

  }

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
  }

  final class NorthConfigTypes extends Config[NorthTypes] {
    override val mirror = edu.gemini.spModel.gemini.gmos.GmosNorthType.DisperserNorth.MIRROR
    override def isMirror(v: GmosNorthType.DisperserNorth): Boolean = v === mirror
  }

  val northConfigTypes: NorthConfigTypes = new NorthConfigTypes

  // This is a trick to allow using a type from a class parameter to define the type of another class parameter
  final case class GmosConfig[T <: SiteDependentTypes] private (
    val cc: Config[T]#CCConfig,
    val dc: DCConfig,
    val c:  Config[T],
    val ns: NSConfig
  ) {
    def this(c: Config[T])(cc: c.CCConfig, dc: DCConfig, ns: NSConfig) = this(cc, dc, c, ns)
  }

  implicit def configShow[T <: SiteDependentTypes]: Show[GmosConfig[T]] =
    Show.show { config =>
      val ccShow =
        if (config.cc.isDarkOrBias) "DarkOrBias"
        else
          s"${config.cc.filter}, ${config.cc.disperser}, ${config.cc.fpu}, ${config.cc.stage}, ${config.cc.stage}, ${config.cc.dtaX}, ${config.cc.adc}, ${config.cc.useElectronicOffset}"

      s"($ccShow, ${config.dc.t}, ${config.dc.s}, ${config.dc.bi}, ${config.dc.roi.rois} ${config.ns})"
    }

}
