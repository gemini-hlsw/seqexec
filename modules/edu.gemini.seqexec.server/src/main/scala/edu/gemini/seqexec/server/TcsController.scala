package edu.gemini.seqexec.server

import edu.gemini.spModel.core.{Wavelength, Offset}

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

  sealed trait M2GuideConfig

  object M2GuideOff extends M2GuideConfig
  
  sealed trait ComaOption
  object ComaOn extends ComaOption
  object ComaOff extends ComaOption

  final case class M2GuideOn(coma: ComaOption, source: Set[TipTiltSource]) extends M2GuideConfig

  sealed trait M1GuideConfig

  object M1GuideOff extends M1GuideConfig

  final case class M1GuideOn(source: M1Source) extends M2GuideConfig

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

  // If x is of type ActiveNodChopTracking then ∃ a:NodChop ∍ x.get(a)
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

    import LightSource._

    object Parked extends ScienceFoldPosition

    final case class Position(source: LightSource, sink: Instrument) extends ScienceFoldPosition {
      val sfPositionName: String = source match {
        case Sky => sink.sfName
        case AO => "ao2" + sink.sfName
        case GCAL => "gcal2" + sink.sfName
      }
    }

  }

  // Offloading of tip/tilt corrections from M2 to mount
  sealed trait MountGuideOption
  object MountGuideOff extends MountGuideOption
  object MountGuideOn extends MountGuideOption

  final case class GuideConfig(mountGuide: MountGuideOption, m1Guide: M1GuideConfig, m2Guide: M2GuideConfig)

  final case class OffsetA(self: Offset) extends AnyVal
  final case class OffsetB(self: Offset) extends AnyVal
  final case class OffsetC(self: Offset) extends AnyVal

  final case class WavelengthA(self: Wavelength) extends AnyVal
  final case class WavelengthB(self: Wavelength) extends AnyVal
  final case class WavelengthC(self: Wavelength) extends AnyVal

  final case class TelescopeConfig(offsetA: OffsetA, offsetB: OffsetB, offsetC: OffsetC,
                                   wavelA: WavelengthA, wavelB: WavelengthB, wavelC: WavelengthC,
                                   m2beam: Beam)

  final case class ProbeTrackingConfigP1(self: ProbeTrackingConfig) extends AnyVal
  final case class ProbeTrackingConfigP2(self: ProbeTrackingConfig) extends AnyVal
  final case class ProbeTrackingConfigOI(self: ProbeTrackingConfig) extends AnyVal
  final case class ProbeTrackingConfigAO(self: ProbeTrackingConfig) extends AnyVal

  final case class GuidersTrackingConfig(pwfs1: ProbeTrackingConfigP1, pwfs2: ProbeTrackingConfigP2,
                                         oiwfs: ProbeTrackingConfigOI, aowfs: ProbeTrackingConfigAO)

  sealed trait GuiderSensorOption
  object GuiderSensorOff extends GuiderSensorOption
  object GuiderSensorOn extends GuiderSensorOption

  final case class GuiderSensorOptionP1(self: GuiderSensorOption) extends AnyVal
  final case class GuiderSensorOptionP2(self: GuiderSensorOption) extends AnyVal
  final case class GuiderSensorOptionOI(self: GuiderSensorOption) extends AnyVal
  final case class GuiderSensorOptionAO(self: GuiderSensorOption) extends AnyVal

  // A enabled guider means it is taking images and producing optical error measurements.
  final case class GuidersEnabled(pwfs1: GuiderSensorOptionP1, pwfs2: GuiderSensorOptionP2,
                                  oiwfs: GuiderSensorOptionOI, aowfs: GuiderSensorOptionAO)

  final case class AGConfig(sfPos: ScienceFoldPosition, hrwfsPos: HrwfsPickupPosition)

  final case class TcsConfig(gc: GuideConfig, tc: TelescopeConfig, gtc: GuidersTrackingConfig, ge: GuidersEnabled,
                             agc: AGConfig)

}
