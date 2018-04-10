// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.tcs

import cats.data.NonEmptyList
import edu.gemini.seqexec.model.Model.Resource
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.tcs.TcsController._
import edu.gemini.seqexec.server.{ConfigResult, SeqAction, System}
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.seqcomp.SeqConfigNames.TELESCOPE_KEY
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import org.log4s.getLogger
import squants.space.Millimeters

import scala.language.implicitConversions
import scala.reflect.ClassTag
import cats._
import cats.implicits._
import mouse.all._

final case class Tcs(tcsController: TcsController, subsystems: NonEmptyList[Subsystem], scienceFoldPosition: ScienceFoldPosition) extends System {

  import Tcs._
  import MountGuideOption._

  override val resource: Resource = Resource.TCS

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
      if (s0.tc.offsetA === s1.self.tc.offsetA) computeGuideOff(s0, s1)
      else GuideConfig(MountGuideOff, M1GuideOff, M2GuideOff)
    }

  // Helper function to output the part of the TCS configuration that is actually applied.
  private def subsystemConfig(tcs: TcsConfig, subsystem: Subsystem): List[AnyRef] = subsystem match {
    case Subsystem.M1 => List(tcs.gc.m1Guide.show)
    case Subsystem.M2 => List(tcs.gc.m2Guide.show)
    case Subsystem.OIWFS => List(tcs.gtc.oiwfs.show, tcs.ge.oiwfs.show)
    case Subsystem.P1WFS => List(tcs.gtc.pwfs1.show, tcs.ge.pwfs1.show)
    case Subsystem.P2WFS => List(tcs.gtc.pwfs2.show, tcs.ge.pwfs2.show)
    case Subsystem.Mount => List(tcs.tc.show)
    case Subsystem.HRProbe => List(tcs.agc.hrwfsPos.show)
    case Subsystem.ScienceFold => List(tcs.agc.sfPos.show)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def configure(config: Config, tcsState: TcsConfig): SeqAction[ConfigResult] = {
    val configFromSequence = fromSequenceConfig(config, subsystems)(tcsState)
    //The desired science fold position is passed as a class parameter
    val tcsConfig = configFromSequence.copy(agc = configFromSequence.agc.copy(sfPos = scienceFoldPosition.some))

    Log.debug(s"Applying TCS configuration: ${subsystems.toList.flatMap(subsystemConfig(tcsConfig, _))}")

    if (subsystems.toList.contains(Subsystem.Mount))
      for {
        _ <- guideOff(tcsState, Requested(tcsConfig))
        _ <- tcsController.applyConfig(subsystems, tcsConfig)
        _ <- tcsController.guide(tcsConfig.gc)
      } yield ConfigResult(this)
    else
      tcsController.applyConfig(subsystems, tcsConfig).map(_ => ConfigResult(this))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  override def configure(config: Config): SeqAction[ConfigResult] = tcsController.getConfig.flatMap(configure(config, _))

  override def notifyObserveStart: SeqAction[Unit] = tcsController.notifyObserveStart

  override def notifyObserveEnd: SeqAction[Unit] = tcsController.notifyObserveEnd
}

object Tcs {
  private val Log = getLogger

  // Shouldn't these be defined somewhere ?
  val GUIDE_WITH_PWFS1_PROP: String = "guideWithPWFS1"
  val GUIDE_WITH_PWFS2_PROP: String = "guideWithPWFS2"
  val GUIDE_WITH_AOWFS_PROP: String = "guideWithAOWFS"
  val P_OFFSET_PROP: String = "p"
  val Q_OFFSET_PROP: String = "q"

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

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def build[T, P: ClassTag](f: P => Endo[T], k: ItemKey, config: Config): Endo[T] =
    config.extract(k).as[P].map(f).foldK

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def build[T, P: ClassTag, Q: ClassTag](f: (P, Q) => Endo[T], k1: ItemKey, k2: ItemKey, config: Config): Endo[T] =
    config.extract(k1).as[P].product(config.extract(k2).as[Q]).map(f.tupled).foldK

  // Parameter specific build functions
  def buildPwfs1Config(guideWithPWFS1: StandardGuideOptions.Value): Endo[TcsConfig] = { s0 =>
    s0.setGuidersTrackingConfig(s0.gtc.setPwfs1TrackingConfig(guideWithPWFS1)).
      setGuidersEnabled(s0.ge.setPwfs1GuiderSensorOption(guideWithPWFS1))
  }

  def buildPwfs2Config(guideWithPWFS2: StandardGuideOptions.Value): Endo[TcsConfig] = { s0 =>
    s0.setGuidersTrackingConfig(s0.gtc.setPwfs2TrackingConfig(guideWithPWFS2)).
      setGuidersEnabled(s0.ge.setPwfs1GuiderSensorOption(guideWithPWFS2))
  }

  def buildOiwfsConfig(guideWithOIWFS: StandardGuideOptions.Value): Endo[TcsConfig] = { s0 =>
    s0.setGuidersTrackingConfig(s0.gtc.setOiwfsTrackingConfig(guideWithOIWFS)).
      setGuidersEnabled(s0.ge.setOiwfsGuiderSensorOption(guideWithOIWFS))
  }

  def buildOffsetConfig(pstr: String, qstr: String): Endo[TcsConfig] = { s0 =>
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

  def fromSequenceConfig(config: Config, subsystems: NonEmptyList[Subsystem])(s0: TcsConfig): TcsConfig = {
    val subs = subsystems.toList
    List(
      subs.contains(Subsystem.P1WFS).option(build(buildPwfs1Config, TELESCOPE_KEY / GUIDE_WITH_PWFS1_PROP, config)),
      subs.contains(Subsystem.P2WFS).option(build(buildPwfs2Config, TELESCOPE_KEY / GUIDE_WITH_PWFS2_PROP, config)),
      subs.contains(Subsystem.OIWFS).option(build(buildOiwfsConfig, TELESCOPE_KEY / GUIDE_WITH_OIWFS_PROP, config)),
      subs.contains(Subsystem.Mount).option(build(buildOffsetConfig, TELESCOPE_KEY / P_OFFSET_PROP, TELESCOPE_KEY / Q_OFFSET_PROP, config))
    ).collect{case Some(x) => x }.foldK.apply(s0)
  }

}
