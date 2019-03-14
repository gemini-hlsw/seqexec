// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.data.{NonEmptySet, OneAnd}
import cats.effect.IO
import cats.implicits._
import seqexec.server.{InstrumentSystem, SeqAction}
import edu.gemini.spModel.core.Wavelength
import gem.enum.LightSinkName
import squants.{Angle, Length}
import monocle.macros.Lenses
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.Gems
import seqexec.server.gems.GemsController.GemsConfig
import shapeless.tag
import shapeless.tag.@@

/**
 * Created by jluhrs on 7/30/15.
 *
 * Interface to change the TCS state.
 * Most of the code deals with representing the state of the TCS subsystems.
 */

trait TcsController {
  import TcsController._

  def applyConfig(subsystems: NonEmptySet[Subsystem],
                  gaos: Option[Either[Altair[IO], Gems[IO]]],
                  tc: TcsConfig): SeqAction[Unit]

  def notifyObserveStart: SeqAction[Unit]

  def notifyObserveEnd: SeqAction[Unit]
}

// scalastyle:off
object TcsController {

  /** Enumerated type for Tip/Tilt Source. */
  sealed trait TipTiltSource
  object TipTiltSource {
    case object PWFS1 extends TipTiltSource
    case object PWFS2 extends TipTiltSource
    case object OIWFS extends TipTiltSource
    case object GAOS  extends TipTiltSource
  }

  /** Enumerated type for M1 Source. */
  sealed trait M1Source
  object M1Source {
    case object PWFS1 extends M1Source
    case object PWFS2 extends M1Source
    case object OIWFS extends M1Source
    case object GAOS  extends M1Source
    case object HRWFS extends M1Source

    implicit val m1SourceEq: Eq[M1Source] = Eq.fromUniversalEquals
  }

  /** Enumerated type for Coma option. */
  sealed trait ComaOption
  object ComaOption {
    case object ComaOn  extends ComaOption
    case object ComaOff extends ComaOption

    implicit val eq: Eq[ComaOption] = Eq.fromUniversalEquals
  }

  /** Data type for M2 guide config. */
  sealed trait M2GuideConfig
  case object M2GuideOff extends M2GuideConfig
  final case class M2GuideOn(coma: ComaOption, source: Set[TipTiltSource]) extends M2GuideConfig
  object M2GuideOn {
    implicit val eq: Eq[M2GuideOn] = Eq.by(x => (x.coma, x.source))
  }
  object M2GuideConfig {
    implicit val show: Show[M2GuideConfig] = Show.fromToString
    implicit val eq: Eq[M2GuideConfig] = Eq.instance{
      case (M2GuideOff, M2GuideOff)               => true
      case (a@M2GuideOn(_, _), b@M2GuideOn(_, _)) => a === b
      case _                                      => false
    }
  }

  /** Data type for M2 guide config. */
  sealed trait M1GuideConfig
  case object M1GuideOff extends M1GuideConfig
  final case class M1GuideOn(source: M1Source) extends M1GuideConfig
  object M1GuideOn {
    implicit val eq: Eq[M1GuideOn] = Eq.by(_.source)
  }
  object M1GuideConfig {
    implicit val show: Show[M1GuideConfig] = Show.fromToString
    implicit val eq: Eq[M1GuideConfig] = Eq.instance{
      case (M1GuideOff, M1GuideOff)         => true
      case (a@M1GuideOn(_), b@M1GuideOn(_)) => a === b
      case _                                => false
    }
  }

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

    implicit val eq: Eq[HrwfsPickupPosition] = Eq.fromUniversalEquals
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

  /** Enumerated type for offloading of tip/tilt corrections from M2 to mount. */
  sealed trait MountGuideOption
  object MountGuideOption {
    case object MountGuideOff extends MountGuideOption
    case object MountGuideOn  extends MountGuideOption

    implicit val eq: Eq[MountGuideOption] = Eq.fromUniversalEquals
  }

  /** Data type for guide config. */
  @Lenses
  final case class TelescopeGuideConfig(mountGuide: MountGuideOption, m1Guide: M1GuideConfig, m2Guide: M2GuideConfig)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object TelescopeGuideConfig {
    implicit val eq: Eq[TelescopeGuideConfig] = Eq.by(x => (x.mountGuide, x.m1Guide, x.m2Guide))
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

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
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

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object GuiderConfig {
    implicit val show: Show[GuiderConfig] = Show.fromToString[GuiderConfig]

    implicit val eq: Eq[GuiderConfig] = Eq.by(x => (x.tracking, x.detector))
  }

  @Lenses
  final case class GuidersConfig(
    pwfs1: GuiderConfig@@P1Config,
    pwfs2OrAowfs: Either[GuiderConfig@@P2Config, ProbeTrackingConfig@@AoGuide],
    oiwfs: GuiderConfig@@OIConfig
  )

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object GuidersConfig {
    implicit val pwfs1Eq: Eq[GuiderConfig@@P1Config] = Eq[GuiderConfig].contramap(identity)
  }

  final case class AGConfig(sfPos: LightPath, hrwfs: Option[HrwfsConfig])

  @Lenses
  final case class TcsConfig(
    gc:  TelescopeGuideConfig,
    tc:  TelescopeConfig,
    gds: GuidersConfig,
    agc: AGConfig,
    gaos: Option[Either[AltairConfig, GemsConfig]],
    inst: InstrumentSystem[IO]
  )

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object TcsConfig

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

}
// scalastyle:on
