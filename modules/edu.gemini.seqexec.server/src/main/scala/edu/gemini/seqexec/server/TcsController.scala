package edu.gemini.seqexec.server

import edu.gemini.spModel.core.Wavelength

import scalaz.{\/-, \/}

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

  sealed trait TipTiltSource

  object TipTiltSource {

    object PWFS1 extends TipTiltSource
    object PWFS2 extends TipTiltSource
    object OIWFS extends TipTiltSource
    object GAOS extends TipTiltSource

  }

  sealed trait M1Source

  object M1Source {

    object PWFS1 extends M1Source
    object PWFS2 extends M1Source
    object OIWFS extends M1Source
    object GAOS extends M1Source
    object HRWFS extends M1Source

  }

  sealed trait M2GuideConfig { val active: Boolean }

  object M2GuideOff extends M2GuideConfig { override val active = false }
  
  sealed case class ComaOption(active: Boolean)
  object ComaOn extends ComaOption(true)
  object ComaOff extends ComaOption(false)

  final case class M2GuideOn(coma: ComaOption, source: Set[TipTiltSource]) extends M2GuideConfig {
    override val active = true
    def setComa(v: ComaOption) = M2GuideOn(v, source)
    def setSource(v: Set[TipTiltSource]) = M2GuideOn(coma, v)
  }

  sealed trait M1GuideConfig { val active: Boolean }

  object M1GuideOff extends M1GuideConfig { override val active = false }

  final case class M1GuideOn(source: M1Source) extends M1GuideConfig { override val active = true }

  sealed trait Beam

  object Beam {

    object A extends Beam
    object B extends Beam
    object C extends Beam

  }

  // Combined configuration of nod position (telescope orientation) and chop position (M2 orientation)
  final case class NodChop(nod: Beam, chop: Beam)
  
  sealed trait NodChopTrackingOption
  object NodChopTrackingOn extends NodChopTrackingOption
  object NodChopTrackingOff extends NodChopTrackingOption

  // TCS can be configured to update a guide probe position only for certain nod-chop positions.
  sealed trait NodChopTrackingConfig {
    def get(nodchop: NodChop): NodChopTrackingOption
  }

  // If x is of type ActiveNodChopTracking then ∃ a:NodChop ∍ x.get(a) == NodChopTrackingOn
  // How could I reflect that in the code?
  sealed trait ActiveNodChopTracking extends NodChopTrackingConfig

  object NodChopTrackingConfig {

    object None extends NodChopTrackingConfig {
      def get(nodchop: NodChop): NodChopTrackingOption = NodChopTrackingOff
    }

    object Normal extends ActiveNodChopTracking {
      def get(nodchop: NodChop): NodChopTrackingOption = {
        if (nodchop.nod == nodchop.chop) NodChopTrackingOn
        else NodChopTrackingOff
      }
    }

    // Must be a non-empty set
    final case class Special(s: Set[NodChop]) extends ActiveNodChopTracking {
      require(s.nonEmpty)
      def get(nodchop: NodChop): NodChopTrackingOption = {
        if(s(nodchop)) NodChopTrackingOn
        else NodChopTrackingOff
      }
    }

  }

  // A probe tracking configuration is specified by its nod-chop tracking table
  // and its follow flag. The first tells TCS when to update the target track
  // followed by the probe, and the second tells the probe if it must follow
  // the target track.

  sealed trait FollowOption
  object FollowOff extends FollowOption
  object FollowOn extends FollowOption

  sealed trait ProbeTrackingConfig {
    def follow: FollowOption

    def getNodChop: NodChopTrackingConfig
  }

  object ProbeTrackingConfig {

    object Parked extends ProbeTrackingConfig {
      override def follow: FollowOption = FollowOff

      override def getNodChop: NodChopTrackingConfig = NodChopTrackingConfig.None
    }

    object Off extends ProbeTrackingConfig {
      override def follow: FollowOption = FollowOff

      override def getNodChop: NodChopTrackingConfig = NodChopTrackingConfig.None
    }

    final case class On(ndconfig: ActiveNodChopTracking) extends ProbeTrackingConfig {
      override def follow: FollowOption = FollowOn

      override def getNodChop: NodChopTrackingConfig = ndconfig
    }

  }

  sealed trait HrwfsPickupPosition

  object HrwfsPickupPosition {

    object IN extends HrwfsPickupPosition
    object OUT extends HrwfsPickupPosition
    object Parked extends HrwfsPickupPosition

  }

  sealed trait LightSource

  object LightSource {

    object Sky extends LightSource
    object AO extends LightSource
    object GCAL extends LightSource

  }

  sealed trait ScienceFoldPosition

  object ScienceFoldPosition {


    object Parked extends ScienceFoldPosition

    final case class Position(source: LightSource, sink: Instrument) extends ScienceFoldPosition

  }

  // Offloading of tip/tilt corrections from M2 to mount
  sealed trait MountGuideOption { val active: Boolean }
  object MountGuideOff extends MountGuideOption { override val active = false }
  object MountGuideOn extends MountGuideOption { override val active = true }

  final case class GuideConfig(mountGuide: MountGuideOption, m1Guide: M1GuideConfig, m2Guide: M2GuideConfig) {
    def setMountGuide(v: MountGuideOption) = GuideConfig(v, m1Guide, m2Guide)
    def setM1Guide(v: M1GuideConfig) = GuideConfig(mountGuide, v, m2Guide)
    def setM2Guide(v: M2GuideConfig) = GuideConfig(mountGuide, m1Guide, v)
  }

  // TCS expects offsets as two length quantities (in millimeters) in the focal plane
  final case class OffsetX(self: Length) extends AnyVal
  final case class OffsetY(self: Length) extends AnyVal
  final case class FocalPlaneOffset(x: OffsetX, y: OffsetY)

  final case class OffsetA(self: FocalPlaneOffset) extends AnyVal
  final case class OffsetB(self: FocalPlaneOffset) extends AnyVal
  final case class OffsetC(self: FocalPlaneOffset) extends AnyVal

  // The WavelengthX classes cannot be value classes, because Wavelength is now a value class, and they cannot be
  // nested.
  final case class WavelengthA(self: Wavelength)
  final case class WavelengthB(self: Wavelength)
  final case class WavelengthC(self: Wavelength)

  final case class TelescopeConfig(offsetA: OffsetA, offsetB: OffsetB, offsetC: OffsetC,
                                   wavelA: WavelengthA, wavelB: WavelengthB, wavelC: WavelengthC,
                                   m2beam: Beam) {
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

  final case class GuidersTrackingConfig(pwfs1: ProbeTrackingConfigP1, pwfs2: ProbeTrackingConfigP2,
                                         oiwfs: ProbeTrackingConfigOI, aowfs: ProbeTrackingConfigAO) {
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
  final case class GuidersEnabled(pwfs1: GuiderSensorOptionP1, pwfs2: GuiderSensorOptionP2,
                                  oiwfs: GuiderSensorOptionOI, aowfs: GuiderSensorOptionAO) {
    def setPwfs1GuiderSensorOption(v: GuiderSensorOption) = GuidersEnabled(GuiderSensorOptionP1(v), pwfs2, oiwfs, aowfs)
    def setPwfs2GuiderSensorOption(v: GuiderSensorOption) = GuidersEnabled(pwfs1, GuiderSensorOptionP2(v), oiwfs, aowfs)
    def setOiwfsGuiderSensorOption(v: GuiderSensorOption) = GuidersEnabled(pwfs1, pwfs2, GuiderSensorOptionOI(v), aowfs)
    def setAowfsGuiderSensorOption(v: GuiderSensorOption) = GuidersEnabled(pwfs1, pwfs2, oiwfs, GuiderSensorOptionAO(v))
  }

  final case class AGConfig(sfPos: ScienceFoldPosition, hrwfsPos: HrwfsPickupPosition)

  final case class InstrumentAlignAngle(self: Angle) extends AnyVal

  final case class TcsConfig(gc: GuideConfig, tc: TelescopeConfig, gtc: GuidersTrackingConfig, ge: GuidersEnabled,
                             agc: AGConfig, iaa: InstrumentAlignAngle) {
    def setGuideConfig(v: GuideConfig) = TcsConfig(v, tc, gtc, ge, agc, iaa)
    def setTelescopeConfig(v: TelescopeConfig) = TcsConfig(gc, v, gtc, ge, agc, iaa)
    def setGuidersTrackingConfig(v: GuidersTrackingConfig) = TcsConfig(gc, tc, v, ge, agc, iaa)
    def setGuidersEnabled(v: GuidersEnabled) = TcsConfig(gc, tc, gtc, v, agc, iaa)
    def setAGConfig(v: AGConfig) = TcsConfig(gc, tc, gtc, ge, v, iaa)
    def setIAA(v: InstrumentAlignAngle) = TcsConfig(gc, tc, gtc, ge, agc, v)
  }

}
