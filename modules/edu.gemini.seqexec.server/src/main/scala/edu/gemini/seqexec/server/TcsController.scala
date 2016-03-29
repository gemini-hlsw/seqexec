package edu.gemini.seqexec.server

import edu.gemini.spModel.core.Wavelength

import scalaz._, Scalaz._

import squants.{Length, Angle}

/**
 * Created by jluhrs on 7/30/15.
 *
 * Interface to change and retrieve the TCS state.
 * Most of the code deals with representing the state of the TCS subsystems.
 */

trait TcsController {
  import TcsController._

  def getConfig: SeqAction[TcsConfig]

  def guide(gc: GuideConfig): SeqAction[Unit]

  def applyConfig(tc: TelescopeConfig, gtc: GuidersTrackingConfig, ge: GuidersEnabled, agc: AGConfig): SeqAction[Unit]

}

object TcsController {

  case class Requested[T](self: T) extends AnyVal

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
  }

  /** Enumerated type for Coma option. */
  sealed trait ComaOption
  object ComaOption {
    case object ComaOn  extends ComaOption
    case object ComaOff extends ComaOption
  }

  /** Data type for M2 guide config. */
  sealed trait M2GuideConfig
  case object M2GuideOff extends M2GuideConfig
  final case class M2GuideOn(coma: ComaOption, source: Set[TipTiltSource]) extends M2GuideConfig {
    def setComa(v: ComaOption) = M2GuideOn(v, source)
    def setSource(v: Set[TipTiltSource]) = M2GuideOn(coma, v)
  }

  /** Data type for M2 guide config. */
  sealed trait M1GuideConfig
  case object M1GuideOff extends M1GuideConfig
  final case class M1GuideOn(source: M1Source) extends M1GuideConfig

  /** Enumerated type for beams A, B, and C. */
  sealed trait Beam
  object Beam {
    case object A extends Beam
    case object B extends Beam
    case object C extends Beam
  }

  /** 
   * Data type for combined configuration of nod position (telescope orientation) and chop position 
   * (M2 orientation)
   */
  final case class NodChop(nod: Beam, chop: Beam)
  object NodChop {
    implicit def EqualNodChop: Equal[NodChop] = 
      Equal.equalA
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
  sealed trait ActiveNodChopTracking extends NodChopTrackingConfig {
  
    // If x is of type ActiveNodChopTracking then ∃ a:NodChop ∍ x.get(a) == NodChopTrackingOn
    // How could I reflect that in the code?

  }
  object NodChopTrackingConfig {

    object None extends NodChopTrackingConfig {
      def get(nodchop: NodChop): NodChopTrackingOption = 
        NodChopTrackingOff
    }

    object Normal extends ActiveNodChopTracking {
      def get(nodchop: NodChop): NodChopTrackingOption =
        NodChopTrackingOption.fromBoolean(nodchop.nod == nodchop.chop)
    }

    final case class Special(s: OneAnd[Set, NodChop]) extends ActiveNodChopTracking {
      def get(nodchop: NodChop): NodChopTrackingOption =
        NodChopTrackingOption.fromBoolean(s.element(nodchop))
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
    final case class Position(source: LightSource, sink: Instrument) extends ScienceFoldPosition
  }

  /** Enumerated type for offloading of tip/tilt corrections from M2 to mount. */
  sealed trait MountGuideOption
  object MountGuideOption {
    case object MountGuideOff extends MountGuideOption
    case object MountGuideOn  extends MountGuideOption
  }
  import MountGuideOption._

  /** Data type for guide config. */
  final case class GuideConfig(mountGuide: MountGuideOption, m1Guide: M1GuideConfig, m2Guide: M2GuideConfig) {
    def setMountGuide(v: MountGuideOption) = GuideConfig(v, m1Guide, m2Guide)
    def setM1Guide(v: M1GuideConfig) = GuideConfig(mountGuide, v, m2Guide)
    def setM2Guide(v: M2GuideConfig) = GuideConfig(mountGuide, m1Guide, v)
  }

  // TCS expects offsets as two length quantities (in millimeters) in the focal plane
  final case class OffsetX(self: Length) extends AnyVal
  object OffsetX {
    implicit val EqualOffsetX: Equal[OffsetX] =
      Equal.equalA // natural equality here
  }

  final case class OffsetY(self: Length) extends AnyVal
  object OffsetY {
    implicit val EqualOffsetY: Equal[OffsetY] =
      Equal.equalA // natural equality here
  }

  final case class FocalPlaneOffset(x: OffsetX, y: OffsetY)
  object FocalPlaneOffset {
    implicit val EqualFocalPlaneOffset: Equal[FocalPlaneOffset] =
      Equal.equalBy(o => (o.x, o.y))
  }

  final case class OffsetA(self: FocalPlaneOffset) extends AnyVal
  object OffsetA {
    implicit val EqualOffsetA: Equal[OffsetA] =
      Equal.equalBy(_.self)
  }

  final case class OffsetB(self: FocalPlaneOffset) extends AnyVal
  object OffsetB {
    implicit val EqualOffsetB: Equal[OffsetB] =
      Equal.equalBy(_.self)
  }

  final case class OffsetC(self: FocalPlaneOffset) extends AnyVal
  object OffsetC {
    implicit val EqualOffsetC: Equal[OffsetC] =
      Equal.equalBy(_.self)
  }

  // The WavelengthX classes cannot be value classes, because Wavelength is now a value class, and they cannot be
  // nested.
  final case class WavelengthA(self: Wavelength)
  final case class WavelengthB(self: Wavelength)
  final case class WavelengthC(self: Wavelength)

  final case class TelescopeConfig(
    offsetA: OffsetA, offsetB: OffsetB, offsetC: OffsetC,
    wavelA:  WavelengthA, wavelB: WavelengthB, wavelC: WavelengthC,
    m2beam: Beam
  ) {

    // TODO: these in terms of .copy
    def setOffsetA(v: FocalPlaneOffset) = TelescopeConfig(OffsetA(v), offsetB, offsetC, wavelA, wavelB, wavelC, m2beam)
    def setOffsetB(v: FocalPlaneOffset) = TelescopeConfig(offsetA, OffsetB(v), offsetC, wavelA, wavelB, wavelC, m2beam)
    def setOffsetC(v: FocalPlaneOffset) = TelescopeConfig(offsetA, offsetB, OffsetC(v), wavelA, wavelB, wavelC, m2beam)
    def setWavelengthA(v: Wavelength) = TelescopeConfig(offsetA, offsetB, offsetC, WavelengthA(v), wavelB, wavelC, m2beam)
    def setWavelengthB(v: Wavelength) = TelescopeConfig(offsetA, offsetB, offsetC, wavelA, WavelengthB(v), wavelC, m2beam)
    def setWavelengthC(v: Wavelength) = TelescopeConfig(offsetA, offsetB, offsetC, wavelA, wavelB, WavelengthC(v), m2beam)
    def setBeam(v: Beam) = TelescopeConfig(offsetA, offsetB, offsetC, wavelA, wavelB, wavelC, v)
  }

  final case class ProbeTrackingConfigP1(self: ProbeTrackingConfig) extends AnyVal
  final case class ProbeTrackingConfigP2(self: ProbeTrackingConfig) extends AnyVal
  final case class ProbeTrackingConfigOI(self: ProbeTrackingConfig) extends AnyVal
  final case class ProbeTrackingConfigAO(self: ProbeTrackingConfig) extends AnyVal

  final case class GuidersTrackingConfig(
    pwfs1: ProbeTrackingConfigP1, 
    pwfs2: ProbeTrackingConfigP2,
    oiwfs: ProbeTrackingConfigOI, 
    aowfs: ProbeTrackingConfigAO
  ) {
    def setPwfs1TrackingConfig(v: ProbeTrackingConfig) = GuidersTrackingConfig(ProbeTrackingConfigP1(v), pwfs2, oiwfs, aowfs)
    def setPwfs2TrackingConfig(v: ProbeTrackingConfig) = GuidersTrackingConfig(pwfs1, ProbeTrackingConfigP2(v), oiwfs, aowfs)
    def setOiwfsTrackingConfig(v: ProbeTrackingConfig) = GuidersTrackingConfig(pwfs1, pwfs2, ProbeTrackingConfigOI(v), aowfs)
    def setAowfsTrackingConfig(v: ProbeTrackingConfig) = GuidersTrackingConfig(pwfs1, pwfs2, oiwfs, ProbeTrackingConfigAO(v))
  }

  sealed trait GuiderSensorOption
  object GuiderSensorOff extends GuiderSensorOption
  object GuiderSensorOn extends GuiderSensorOption

  final case class GuiderSensorOptionP1(self: GuiderSensorOption) extends AnyVal
  final case class GuiderSensorOptionP2(self: GuiderSensorOption) extends AnyVal
  final case class GuiderSensorOptionOI(self: GuiderSensorOption) extends AnyVal
  final case class GuiderSensorOptionAO(self: GuiderSensorOption) extends AnyVal

  // A enabled guider means it is taking images and producing optical error measurements.
  final case class GuidersEnabled(
    pwfs1: GuiderSensorOptionP1, 
    pwfs2: GuiderSensorOptionP2,
    oiwfs: GuiderSensorOptionOI, 
    aowfs: GuiderSensorOptionAO
  ) {
    def setPwfs1GuiderSensorOption(v: GuiderSensorOption) = GuidersEnabled(GuiderSensorOptionP1(v), pwfs2, oiwfs, aowfs)
    def setPwfs2GuiderSensorOption(v: GuiderSensorOption) = GuidersEnabled(pwfs1, GuiderSensorOptionP2(v), oiwfs, aowfs)
    def setOiwfsGuiderSensorOption(v: GuiderSensorOption) = GuidersEnabled(pwfs1, pwfs2, GuiderSensorOptionOI(v), aowfs)
    def setAowfsGuiderSensorOption(v: GuiderSensorOption) = GuidersEnabled(pwfs1, pwfs2, oiwfs, GuiderSensorOptionAO(v))
  }

  final case class AGConfig(sfPos: ScienceFoldPosition, hrwfsPos: HrwfsPickupPosition)

  final case class InstrumentAlignAngle(self: Angle) extends AnyVal

  final case class TcsConfig(
    gc:  GuideConfig, 
    tc:  TelescopeConfig, 
    gtc: GuidersTrackingConfig, 
    ge:  GuidersEnabled,
    agc: AGConfig, 
    iaa: InstrumentAlignAngle
  ) {
    def setGuideConfig(v: GuideConfig) = TcsConfig(v, tc, gtc, ge, agc, iaa)
    def setTelescopeConfig(v: TelescopeConfig) = TcsConfig(gc, v, gtc, ge, agc, iaa)
    def setGuidersTrackingConfig(v: GuidersTrackingConfig) = TcsConfig(gc, tc, v, ge, agc, iaa)
    def setGuidersEnabled(v: GuidersEnabled) = TcsConfig(gc, tc, gtc, v, agc, iaa)
    def setAGConfig(v: AGConfig) = TcsConfig(gc, tc, gtc, ge, v, iaa)
    def setIAA(v: InstrumentAlignAngle) = TcsConfig(gc, tc, gtc, ge, agc, v)
  }

}
