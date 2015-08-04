package edu.gemini.seqexec.server

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

  case class Offset(p: Double, q: Double)

  object Offset {

    object Null extends Offset(0.0, 0.0)

  }

  type Wavelength = Double

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

  case class M2GuideOn(coma: Boolean, source: Set[TipTiltSource]) extends M2GuideConfig

  sealed trait M1GuideConfig

  object M1GuideOff extends M1GuideConfig

  case class M1GuideOn(coma: Boolean, source: Set[M1Source]) extends M2GuideConfig

  sealed trait Beam

  object Beam {

    object A extends Beam

    object B extends Beam

    object C extends Beam

  }

  // Combined configuration of nod position (telescope orientation) and chop position (M2 orientation)
  case class NodChop(nod: Beam, chop: Beam)

  // TCS can be configured to update a guide probe position only for certain nod-chop positions.
  sealed trait NodChopTrackingConfig {
    def get(nodchop: NodChop): Boolean
  }

  // If x is of type ActiveNodChopTracking then ∃ a:NodChop ∍ x.get(a)
  // How could I reflect that in the code?
  sealed trait ActiveNodChopTracking extends NodChopTrackingConfig

  object NodChopTrackingConfig {

    object None extends NodChopTrackingConfig {
      def get(nodchop: NodChop): Boolean = false
    }

    object Normal extends ActiveNodChopTracking {
      def get(nodchop: NodChop): Boolean = nodchop.nod == nodchop.chop
    }

    // Must be a non-empty set
    case class Special(s: Set[NodChop]) extends ActiveNodChopTracking {
      def get(nodchop: NodChop): Boolean = s(nodchop)
    }

  }

  // A probe tracking configuration is specified by its nod-chop tracking table
  // and its follow flag. The first tells TCS when to update the target track
  // followed by the probe, and the second tells the probe when to move
  // following the target track.
  sealed trait ProbeTrackingConfig {
    def follow: Boolean

    def getNodChop: NodChopTrackingConfig
  }

  object ProbeTrackingConfig {

    object Parked extends ProbeTrackingConfig {
      override def follow: Boolean = false

      override def getNodChop: NodChopTrackingConfig = NodChopTrackingConfig.None
    }

    object Off extends ProbeTrackingConfig {
      override def follow: Boolean = false

      override def getNodChop: NodChopTrackingConfig = NodChopTrackingConfig.None
    }

    case class On(ndconfig: ActiveNodChopTracking) extends ProbeTrackingConfig {
      override def follow: Boolean = true

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

    class Position(source: LightSource, sink: Instrument) extends ScienceFoldPosition {
      val sfPositionName: String = source match {
        case Sky => sink.sfName
        case AO => "ao2" + sink.sfName
        case GCAL => "gcal2" + sink.sfName
      }
    }

  }

  case class GuideConfig(mountGuide: Boolean, m1Guide: M1GuideConfig, m2Guide: M2GuideConfig)

  case class TelescopeConfig(offsetA: Offset, offsetB: Offset, offsetC: Offset, wavelA: Wavelength, wavelB: Wavelength,
                             wavelC: Wavelength, m2beam: Beam)

  case class GuidersTrackingConfig(p1wfs: ProbeTrackingConfig, pwfs2: ProbeTrackingConfig, oiwfs: ProbeTrackingConfig, aowfs: ProbeTrackingConfig)

  // A enabled guider means it is taking images and producing optical error measurements.
  case class GuidersEnabled(p1wfs: Boolean, pwfs2: Boolean, oiwfs: Boolean, aowfs: Boolean)

  case class AGConfig(sfPos: ScienceFoldPosition, hrwfsPos: HrwfsPickupPosition)

  case class TcsConfig(gc: GuideConfig, tc: TelescopeConfig, gtc: GuidersTrackingConfig, ge: GuidersEnabled, agc: AGConfig)

}
