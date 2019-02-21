// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.data.{NonEmptyList, OneAnd}
import cats.effect.IO
import cats.implicits._
import seqexec.server.SeqAction
import edu.gemini.spModel.core.Wavelength
import gem.enum.LightSinkName
import squants.Angle
import monocle.macros.Lenses
import seqexec.server.altair.Altair
import seqexec.server.gems.Gems
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

  def applyConfig(subsystems: NonEmptyList[Subsystem],
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
  }

  /** Data type for M2 guide config. */
  sealed trait M2GuideConfig
  object M2GuideConfig {
    implicit val show: Show[M2GuideConfig] = Show.fromToString
  }
  case object M2GuideOff extends M2GuideConfig
  final case class M2GuideOn(coma: ComaOption, source: Set[TipTiltSource]) extends M2GuideConfig

  /** Data type for M2 guide config. */
  sealed trait M1GuideConfig
  object M1GuideConfig {
    implicit val show: Show[M1GuideConfig] = Show.fromToString
  }
  case object M1GuideOff extends M1GuideConfig
  final case class M1GuideOn(source: M1Source) extends M1GuideConfig

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
    implicit def EqNodChop: Eq[NodChop] =
      Eq[(Beam, Beam)].contramap(x => (x.nod, x.chop))
  }

  /** Enumerated type for nod/chop tracking. */
  sealed trait NodChopTrackingOption
  object NodChopTrackingOption {

    case object NodChopTrackingOn  extends NodChopTrackingOption
    case object NodChopTrackingOff extends NodChopTrackingOption

    def fromBoolean(on: Boolean): NodChopTrackingOption =
      if (on) NodChopTrackingOn else NodChopTrackingOff

  }
  import NodChopTrackingOption._ // needed below

  // TCS can be configured to update a guide probe position only for certain nod-chop positions.
  sealed trait NodChopTrackingConfig {
    def get(nodchop: NodChop): NodChopTrackingOption
  }
  sealed trait ActiveNodChopTracking extends NodChopTrackingConfig

    // If x is of type ActiveNodChopTracking then ∃ a:NodChop ∍ x.get(a) == NodChopTrackingOn
    // How could I reflect that in the code?

  object NodChopTrackingConfig {

    object None extends NodChopTrackingConfig {
      def get(nodchop: NodChop): NodChopTrackingOption =
        NodChopTrackingOff
    }

    object Normal extends ActiveNodChopTracking {
      def get(nodchop: NodChop): NodChopTrackingOption =
        NodChopTrackingOption.fromBoolean(nodchop.nod === nodchop.chop)
    }

    final case class Special(s: OneAnd[List, NodChop]) extends ActiveNodChopTracking {
      def get(nodchop: NodChop): NodChopTrackingOption =
        NodChopTrackingOption.fromBoolean(s.exists(_ === nodchop))
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
  )
  object ProbeTrackingConfig {
    case object Parked extends ProbeTrackingConfig(FollowOff, NodChopTrackingConfig.None)
    case object Off extends ProbeTrackingConfig(FollowOff, NodChopTrackingConfig.None)
    final case class On(ndconfig: ActiveNodChopTracking) extends ProbeTrackingConfig(FollowOn, ndconfig)
  }

  /** Enumerated type for HRWFS pickup position. */
  sealed trait HrwfsPickupPosition
  object HrwfsPickupPosition {
    case object IN     extends HrwfsPickupPosition
    case object OUT    extends HrwfsPickupPosition
    case object Parked extends HrwfsPickupPosition
    implicit val show: Show[HrwfsPickupPosition] = Show.fromToString
  }

  sealed trait HrwfsConfig
  object HrwfsConfig {
    case object Auto                                  extends HrwfsConfig
    final case class Manual(pos: HrwfsPickupPosition) extends HrwfsConfig
    implicit val show: Show[HrwfsConfig] = Show.fromToString
  }

  /** Enumerated type for light source. */
  sealed trait LightSource
  object LightSource {
    case object Sky  extends LightSource
    case object AO   extends LightSource
    case object GCAL extends LightSource
  }

  /** Data type for science fold position. */
  sealed trait ScienceFoldPosition
  object ScienceFoldPosition {
    case object Parked extends ScienceFoldPosition
    final case class Position(source: LightSource, sink: LightSinkName) extends ScienceFoldPosition
    implicit val show: Show[ScienceFoldPosition] = Show.fromToString
  }

  /** Enumerated type for offloading of tip/tilt corrections from M2 to mount. */
  sealed trait MountGuideOption
  object MountGuideOption {
    case object MountGuideOff extends MountGuideOption
    case object MountGuideOn  extends MountGuideOption
  }

  /** Data type for guide config. */
  @Lenses
  final case class TelescopeGuideConfig(mountGuide: MountGuideOption, m1Guide: M1GuideConfig, m2Guide: M2GuideConfig)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object TelescopeGuideConfig

  // TCS expects offsets as two length quantities (in millimeters) in the focal plane
  trait OffsetP
  trait OffsetQ

  final case class InstrumentOffset(p: Angle@@OffsetP, q: Angle@@OffsetQ)
  object InstrumentOffset {
    implicit val EqInstrumentOffset: Eq[InstrumentOffset] =
      Eq.by(o => (o.p.value, o.q.value))
  }

  @Lenses
  final case class TelescopeConfig(
    offsetA: Option[InstrumentOffset],
    wavelA:  Option[Wavelength]
  )

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object TelescopeConfig {
    implicit val show: Show[TelescopeConfig] = Show.fromToString
  }

  trait P1Config
  trait P2Config
  trait OIConfig
  trait AOGuide

  sealed trait GuiderSensorOption
  object GuiderSensorOff extends GuiderSensorOption
  object GuiderSensorOn extends GuiderSensorOption
  object GuiderSensorOption {
    implicit val guideSensorOptionEq: Eq[GuiderSensorOption] = Eq.fromUniversalEquals
  }

  @Lenses
  final case class GuiderConfig(tracking: ProbeTrackingConfig, detector: GuiderSensorOption)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object GuiderConfig {
    implicit val show: Show[GuiderConfig] = Show.fromToString[GuiderConfig]
  }

  @Lenses
  final case class GuidersConfig(
    pwfs1: GuiderConfig@@P1Config,
    pwfs2: GuiderConfig@@P2Config,
    oiwfs: GuiderConfig@@OIConfig,
    aowfs: Boolean@@AOGuide
  )

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object GuidersConfig

  final case class AGConfig(sfPos: ScienceFoldPosition, hrwfs: Option[HrwfsConfig])

  @Lenses
  final case class TcsConfig(
    gc:  TelescopeGuideConfig,
    tc:  TelescopeConfig,
    gds: GuidersConfig,
    agc: AGConfig
  )

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object TcsConfig

  val defaultConfig = TcsConfig(
    TelescopeGuideConfig(MountGuideOption.MountGuideOff, M1GuideOff, M2GuideOff),
    TelescopeConfig(None, None),
    GuidersConfig(
      tag[P1Config](GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)),
      tag[P2Config](GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)),
      tag[OIConfig](GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)),
      tag[AOGuide](false)
    ),
    AGConfig(ScienceFoldPosition.Parked, HrwfsConfig.Auto.some)
  )

  sealed trait Subsystem extends Product with Serializable
  object Subsystem {
    // Instrument internal WFS
    case object OIWFS  extends Subsystem
    // Peripheral WFS 1
    case object P1WFS  extends Subsystem
    // Peripheral WFS 2
    case object P2WFS  extends Subsystem
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

    val all: NonEmptyList[Subsystem] = NonEmptyList.of(OIWFS, P1WFS, P2WFS, AGUnit, Mount, M1, M2, Gaos)
    val allButOI: NonEmptyList[Subsystem] = NonEmptyList.of(P1WFS, P2WFS, AGUnit, Mount, M1, M2, Gaos)

    implicit val show: Show[Subsystem] = Show.show { _.productPrefix }
    implicit val equal: Eq[Subsystem] = Eq.fromUniversalEquals
  }

}
// scalastyle:on
