// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.data.{NonEmptySet, OneAnd}
import cats.implicits._
import edu.gemini.spModel.core.Wavelength
import gem.enum._
import monocle.macros.Lenses
import seqexec.model.TelescopeGuideConfig
import seqexec.server.InstrumentGuide
import squants.{Angle, Length}
import shapeless.tag
import shapeless.tag.@@

/**
 * Created by jluhrs on 7/30/15.
 *
 * Most of the code deals with representing the state of the TCS subsystems.
 */

object TcsController {

  /** Enumerated type for beams A, B, and C. */
  sealed trait Beam extends Product with Serializable
  object Beam {
    case object A extends Beam
    case object B extends Beam
    case object C extends Beam

    implicit val equal: Eq[Beam] = Eq.fromUniversalEquals
  }

  /**
   * Data type for combined configuration of nod position (telescope orientation) and chop position
   * (M2 orientation)
   */
  final case class NodChop(nod: Beam, chop: Beam)
  object NodChop {
    implicit def EqNodChop: Eq[NodChop] = Eq.by(x => (x.nod, x.chop))
  }

  /** Enumerated type for nod/chop tracking. */
  sealed trait NodChopTrackingOption
  object NodChopTrackingOption {

    case object NodChopTrackingOn  extends NodChopTrackingOption
    case object NodChopTrackingOff extends NodChopTrackingOption

    def fromBoolean(on: Boolean): NodChopTrackingOption =
      if (on) NodChopTrackingOn else NodChopTrackingOff

    implicit val eq: Eq[NodChopTrackingOption] = Eq.fromUniversalEquals

  }
  import NodChopTrackingOption._ // needed below

  // TCS can be configured to update a guide probe position only for certain nod-chop positions.
  sealed trait NodChopTrackingConfig {
    def get(nodchop: NodChop): NodChopTrackingOption
  }

  // If x is of type ActiveNodChopTracking then ∃ a:NodChop ∍ x.get(a) == NodChopTrackingOn
  // How could I reflect that in the code?
  sealed trait ActiveNodChopTracking extends NodChopTrackingConfig

  object NodChopTrackingConfig {

    object AllOff extends NodChopTrackingConfig {
      def get(nodchop: NodChop): NodChopTrackingOption =
        NodChopTrackingOff
    }

    // Beam C is never used on normal configuration, and setting it to On would cause problems because no other tool
    // (TCC, Tcs engineering screens) can change it.
    object Normal extends ActiveNodChopTracking {
      def get(nodchop: NodChop): NodChopTrackingOption =
        NodChopTrackingOption.fromBoolean(nodchop.nod =!= Beam.C && nodchop.nod === nodchop.chop)
    }

    final case class Special(s: OneAnd[List, NodChop]) extends ActiveNodChopTracking {
      def get(nodchop: NodChop): NodChopTrackingOption =
        NodChopTrackingOption.fromBoolean(s.exists(_ === nodchop))
    }

    implicit val specialEq: Eq[Special] = Eq.by(_.s)

    implicit val activeEq: Eq[ActiveNodChopTracking] = Eq.instance{
      case (Normal, Normal)             => true
      case (a@Special(_), b@Special(_)) => a === b
      case _                            => false
    }

    implicit val eq: Eq[NodChopTrackingConfig] = Eq.instance{
      case (AllOff, AllOff)                                     => true
      case (a: ActiveNodChopTracking, b: ActiveNodChopTracking) => a === b
      case _                                                    => false
    }

  }

  // A probe tracking configuration is specified by its nod-chop tracking table
  // and its follow flag. The first tells TCS when to update the target track
  // followed by the probe, and the second tells the probe if it must follow
  // the target track.

  /** Enumerated type for follow on/off. */
  sealed trait FollowOption
  object FollowOption {
    case object FollowOff extends FollowOption
    case object FollowOn  extends FollowOption
    implicit val followEq: Eq[FollowOption] = Eq.fromUniversalEquals[FollowOption]
  }
  import FollowOption._

  /** Data type for probe tracking config. */
  sealed abstract class ProbeTrackingConfig(
    val follow: FollowOption,
    val getNodChop: NodChopTrackingConfig
  ) {
    def isActive: Boolean = follow === FollowOn && getNodChop =!= NodChopTrackingConfig.AllOff
  }
  object ProbeTrackingConfig {
    case object Parked extends ProbeTrackingConfig(FollowOff, NodChopTrackingConfig.AllOff)
    case object Off extends ProbeTrackingConfig(FollowOff, NodChopTrackingConfig.AllOff)
    final case class On(ndconfig: ActiveNodChopTracking) extends ProbeTrackingConfig(FollowOn, ndconfig)
    case object Frozen extends ProbeTrackingConfig(FollowOn, NodChopTrackingConfig.AllOff)

    implicit val onEq: Eq[On] = Eq.by(_.ndconfig)

    implicit val eq: Eq[ProbeTrackingConfig] = Eq.instance{
      case (Parked, Parked)   => true
      case (Off, Off)         => true
      case (Frozen, Frozen)   => true
      case (a@On(_), b@On(_)) => a === b
      case _                  => false
    }
    implicit val show: Show[ProbeTrackingConfig] = Show.fromToString
  }

  /** Enumerated type for HRWFS pickup position. */
  sealed trait HrwfsPickupPosition
  object HrwfsPickupPosition {
    case object IN     extends HrwfsPickupPosition
    case object OUT    extends HrwfsPickupPosition
    case object Parked extends HrwfsPickupPosition

    implicit val show: Show[HrwfsPickupPosition] = Show.fromToString

    implicit val eq: Eq[HrwfsPickupPosition] = Eq.instance{
      case (IN, IN)         => true
      case (OUT, OUT)       => true
      case (Parked, Parked) => true
      case _                => false
    }

    def isInTheWay(h: HrwfsPickupPosition): Boolean = h === IN
  }

  sealed trait HrwfsConfig
  object HrwfsConfig {
    case object Auto                                  extends HrwfsConfig
    final case class Manual(pos: HrwfsPickupPosition) extends HrwfsConfig

    implicit val show: Show[HrwfsConfig] = Show.fromToString

    implicit val manualEq: Eq[Manual] = Eq.by(_.pos)

    implicit val eq: Eq[HrwfsConfig] = Eq.instance{
      case (Auto, Auto)               => true
      case (a@Manual(_), b@Manual(_)) => a === b
      case _                          => false
    }
  }

  /** Enumerated type for light source. */
  sealed trait LightSource
  object LightSource {
    case object Sky  extends LightSource
    case object AO   extends LightSource
    case object GCAL extends LightSource

    implicit val eq: Eq[LightSource] =  Eq.fromUniversalEquals
  }

  /* Data type for science fold position. */
  final case class LightPath(source: LightSource, sink: LightSinkName)
  object LightPath {

    implicit val show: Show[LightPath] = Show.fromToString

    implicit val positionEq: Eq[LightPath] = Eq.by(x => (x.source, x.sink))
  }

  // TCS expects offsets as two length quantities (in millimeters) in the focal plane
  trait OffsetX
  trait OffsetY

  final case class FocalPlaneOffset(x: Length@@OffsetX, y: Length@@OffsetY) {
    def toInstrumentOffset(iaa: Angle): InstrumentOffset = InstrumentOffset(
      tag[OffsetP]((-x * iaa.cos + y * iaa.sin) * FOCAL_PLANE_SCALE),
      tag[OffsetQ]((-x * iaa.sin - y * iaa.cos) * FOCAL_PLANE_SCALE)
    )
  }
  object FocalPlaneOffset {
    implicit val eq: Eq[FocalPlaneOffset] =  Eq.by(o => (o.x.value, o.y.value))

    def fromInstrumentOffset(o: InstrumentOffset, iaa: Angle): FocalPlaneOffset = o.toFocalPlaneOffset(iaa)
  }

  trait OffsetP
  trait OffsetQ

  final case class InstrumentOffset(p: Angle@@OffsetP, q: Angle@@OffsetQ) {
    def toFocalPlaneOffset(iaa: Angle): FocalPlaneOffset = FocalPlaneOffset(
      tag[OffsetX]((-p * iaa.cos - q * iaa.sin) / FOCAL_PLANE_SCALE),
      tag[OffsetY](( p * iaa.sin - q * iaa.cos) / FOCAL_PLANE_SCALE)
    )
  }
  object InstrumentOffset {
    implicit val EqInstrumentOffset: Eq[InstrumentOffset] =
      Eq.by(o => (o.p.value, o.q.value))

    def fromFocalPlaneOffset(o: FocalPlaneOffset, iaa: Angle): InstrumentOffset = o.toInstrumentOffset(iaa)
  }

  @Lenses
  final case class TelescopeConfig(
    offsetA: Option[InstrumentOffset],
    wavelA:  Option[Wavelength]
  )

  implicit val wavelengthEq: Eq[Wavelength] = Eq.by(_.length.value)

  object TelescopeConfig {
    implicit val show: Show[TelescopeConfig] = Show.fromToString
  }

  trait P1Config
  trait P2Config
  trait OIConfig
  trait AoGuide

  sealed trait GuiderSensorOption
  object GuiderSensorOff extends GuiderSensorOption
  object GuiderSensorOn extends GuiderSensorOption
  object GuiderSensorOption {
    implicit val guideSensorOptionEq: Eq[GuiderSensorOption] = Eq.fromUniversalEquals
  }

  @Lenses
  final case class GuiderConfig(tracking: ProbeTrackingConfig, detector: GuiderSensorOption) {
    val isActive: Boolean = tracking.isActive && detector === GuiderSensorOn
  }

  object GuiderConfig {
    implicit val show: Show[GuiderConfig] = Show.fromToString[GuiderConfig]

    implicit val eq: Eq[GuiderConfig] = Eq.by(x => (x.tracking, x.detector))
  }

  final case class AGConfig(sfPos: LightPath, hrwfs: Option[HrwfsConfig])

  sealed trait Subsystem extends Product with Serializable
  object Subsystem {
    // Instrument internal WFS
    case object OIWFS  extends Subsystem
    // Peripheral WFS 1
    case object PWFS1  extends Subsystem
    // Peripheral WFS 2
    case object PWFS2  extends Subsystem
    // Internal AG mechanisms (science fold, AC arm)
    case object AGUnit extends Subsystem
    // Mount and cass-rotator
    case object Mount  extends Subsystem
    // Primary mirror
    case object M1     extends Subsystem
    // Secondary mirror
    case object M2     extends Subsystem
    // Gemini Adaptive Optics System (GeMS or Altair)
    case object Gaos   extends Subsystem

    val allList: List[Subsystem] = List(PWFS1, PWFS2, OIWFS, AGUnit, Mount, M1, M2, Gaos)
    implicit val order: Order[Subsystem] = Order.from{ case (a, b) => allList.indexOf(a) - allList.indexOf(b) }
    val allButGaos: NonEmptySet[Subsystem] = NonEmptySet.of(OIWFS, PWFS1, PWFS2, AGUnit, Mount, M1, M2)
    val allButGaosNorOi: NonEmptySet[Subsystem] = NonEmptySet.of(PWFS1, PWFS2, AGUnit, Mount, M1, M2)

    implicit val show: Show[Subsystem] = Show.show { _.productPrefix }
    implicit val equal: Eq[Subsystem] = Eq.fromUniversalEquals
  }

  sealed trait GuidersConfig[+C] {
    val pwfs1: GuiderConfig@@P1Config
    val pwfs2: GuiderConfig@@P2Config
    val oiwfs: GuiderConfig@@OIConfig
  }

  @Lenses
  final case class BasicGuidersConfig(
    pwfs1: GuiderConfig@@P1Config,
    pwfs2: GuiderConfig@@P2Config,
    oiwfs: GuiderConfig@@OIConfig
  ) extends GuidersConfig[Nothing]

  @Lenses
  final case class AoGuidersConfig[C](
    pwfs1: GuiderConfig@@P1Config,
    aoguide: C,
    oiwfs: GuiderConfig@@OIConfig
  ) extends GuidersConfig[C] {
    override val pwfs2: GuiderConfig@@P2Config =
      tag[P2Config](GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff))
  }

  sealed trait TcsConfig[+C, +G] {
    val gc:  TelescopeGuideConfig
    val tc:  TelescopeConfig
    val gds: GuidersConfig[C]
    val agc: AGConfig
    val inst: InstrumentGuide
  }

  @Lenses
  final case class BasicTcsConfig(
    gc:  TelescopeGuideConfig,
    tc:  TelescopeConfig,
    gds: BasicGuidersConfig,
    agc: AGConfig,
    inst: InstrumentGuide
  ) extends TcsConfig[Nothing, Nothing]

  @Lenses
  final case class AoTcsConfig[C, G](
    gc:  TelescopeGuideConfig,
    tc:  TelescopeConfig,
    gds: AoGuidersConfig[C],
    agc: AGConfig,
    gaos: G,
    inst: InstrumentGuide
  ) extends TcsConfig[C, G]

}
