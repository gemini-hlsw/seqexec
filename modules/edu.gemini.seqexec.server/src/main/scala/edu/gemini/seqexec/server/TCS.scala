package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.seqexec.server.ConfigUtil._
import edu.gemini.seqexec.server.TcsController._
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.core.{Angle, OffsetP, OffsetQ}
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.seqcomp.SeqConfigNames.{TELESCOPE_CONFIG_NAME, TELESCOPE_KEY}
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.concurrent.Task
import scalaz.{EitherT, \/}
import squants.space.{Millimeters, LengthConversions}

/**
 * Created by jluhrs on 4/23/15.
 */
final case class TCS(tcsController: TcsController) extends System {

  import TCS._
  import MountGuideOption._

  override val name: String = TELESCOPE_CONFIG_NAME

  private def computeGuideOff(s0: TcsConfig, s1: Requested[TcsConfig]): GuideConfig = {

    val g0 = s1.self.gc.mountGuide match {
      case MountGuideOff => s0.gc.setMountGuide(MountGuideOff)
      case _             => s0.gc
    }
    
    val g1 = s1.self.gc.m1Guide match {
      case M1GuideOff => g0.setM1Guide(M1GuideOff)
      case _          => g0
    }

    val g2 = s1.self.gc.m2Guide match {
      case M2GuideOff => g1.setM2Guide(M2GuideOff)
      case _          => g1
    }

    g2 // final result

  }

  private def guideOff(s0: TcsConfig, s1: Requested[TcsConfig]): SeqAction[Unit] =
    tcsController.guide {
      if (s0.tc.offsetA == s1.self.tc.offsetA) computeGuideOff(s0, s1)
      else GuideConfig(MountGuideOff, M1GuideOff, M2GuideOff)
    }

  private def configure(config: Config, tcsState: TcsConfig): SeqAction[ConfigResult] = {
    val tcsConfig = fromSequenceConfig(config)(tcsState)

    for {
      _ <- guideOff(tcsState, Requested(tcsConfig))
      _ <- tcsController.applyConfig(tcsConfig.tc, tcsConfig.gtc, tcsConfig.ge, tcsConfig.agc)
      _ <- tcsController.guide(tcsConfig.gc)
    } yield ConfigResult(this)
  }

  override def configure(config: Config): SeqAction[ConfigResult] = {
    tcsController.getConfig.flatMap(configure(config, _))
  }
}

object TCS {
  private val Log = Logger.getLogger(getClass.getName)

  // Shouldn't these be defined somewhere ?
  val GUIDE_WITH_PWFS1_PROP = "guideWithPWFS1"
  val GUIDE_WITH_PWFS2_PROP = "guideWithPWFS2"
  val P_OFFSET_PROP = "p"
  val Q_OFFSET_PROP = "q"

  // Conversions from ODB model values to TCS configuration values
  implicit def probeTrackingConfigFromGuideWith(guideWith: StandardGuideOptions.Value): ProbeTrackingConfig = guideWith match {
    case StandardGuideOptions.Value.park => ProbeTrackingConfig.Parked
    case StandardGuideOptions.Value.freeze => ProbeTrackingConfig.Off
    case StandardGuideOptions.Value.guide => ProbeTrackingConfig.On(NodChopTrackingConfig.Normal)
  }

  implicit def guideSensorOptionFromGuideWith(guideWith: StandardGuideOptions.Value): GuiderSensorOption = {
    if (guideWith.isActive) GuiderSensorOn
    else GuiderSensorOff
  }

  // Helper functions for incrementally building a TCS configuration
  def buildWithConjunction[T, P, S](f: P => T => T)(p: S \/ P): T => T = p.map(f).getOrElse((t: T) => t)

  def buildWithConjunction[T, P, Q, S](f: (P, Q) => T => T)(p: S \/ P, q: S \/ Q): T => T = ( for {
      r <- p
      s <- q
    } yield f(r, s)
  ).getOrElse((t: T) => t)

  def build[T, P](f: P => T => T, k: ItemKey, config: Config)(implicit clazz: ClassTag[P]): T => T =
    buildWithConjunction(f)(extract[P](config, k)(clazz))

  def build[T, P, Q](f: (P, Q) => T => T, k1: ItemKey, k2: ItemKey, config: Config)(implicit clazzp: ClassTag[P], clazzq: ClassTag[Q]): T => T =
    buildWithConjunction(f)(extract[P](config, k1)(clazzp), extract[Q](config, k2)(clazzq))

  // Parameter specific build functions
  def buildPwfs1Config(guideWithPWFS1: StandardGuideOptions.Value)(s0: TcsConfig): TcsConfig = {
    s0.setGuidersTrackingConfig(s0.gtc.setPwfs1TrackingConfig(guideWithPWFS1)).
      setGuidersEnabled(s0.ge.setPwfs1GuiderSensorOption(guideWithPWFS1))
  }

  def buildPwfs2Config(guideWithPWFS2: StandardGuideOptions.Value)(s0: TcsConfig): TcsConfig = {
    s0.setGuidersTrackingConfig(s0.gtc.setPwfs2TrackingConfig(guideWithPWFS2)).
      setGuidersEnabled(s0.ge.setPwfs1GuiderSensorOption(guideWithPWFS2))
  }

  def buildOiwfsConfig(guideWithOIWFS: StandardGuideOptions.Value)(s0: TcsConfig): TcsConfig = {
    s0.setGuidersTrackingConfig(s0.gtc.setOiwfsTrackingConfig(guideWithOIWFS)).
      setGuidersEnabled(s0.ge.setOiwfsGuiderSensorOption(guideWithOIWFS))
  }

  def buildOffsetConfig(pstr: String, qstr: String)(s0: TcsConfig): TcsConfig = {
    // Is there a way to express this value with squants quantities ?
    val FOCAL_PLANE_SCALE = 1.61144; //[arcsec/mm]

    try {
      val p = pstr.toDouble
      val q = qstr.toDouble
      val x = (-p * s0.iaa.self.cos - q * s0.iaa.self.sin) / FOCAL_PLANE_SCALE
      val y = (p * s0.iaa.self.sin - q * s0.iaa.self.cos) / FOCAL_PLANE_SCALE
      s0.setTelescopeConfig(s0.tc.setOffsetA(FocalPlaneOffset(OffsetX(Millimeters(x)), OffsetY(Millimeters(y)))))
    } catch {
      case _: Throwable => s0
    }
  }

  def fromSequenceConfig(config: Config)(s0: TcsConfig): TcsConfig = {
    List(
      build(buildPwfs1Config, new ItemKey(TELESCOPE_KEY, GUIDE_WITH_PWFS1_PROP), config),
      build(buildPwfs2Config, new ItemKey(TELESCOPE_KEY, GUIDE_WITH_PWFS2_PROP), config),
      build(buildOiwfsConfig, new ItemKey(TELESCOPE_KEY, GUIDE_WITH_OIWFS_PROP), config),
      build(buildOffsetConfig, new ItemKey(TELESCOPE_KEY, P_OFFSET_PROP), new ItemKey(TELESCOPE_KEY, Q_OFFSET_PROP),
        config)
    ).foldLeft(s0)((b, f) => f(b))

  }

}