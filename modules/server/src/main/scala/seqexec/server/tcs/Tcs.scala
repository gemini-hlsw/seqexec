// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.syntax.all._
import edu.gemini.spModel.guide.StandardGuideOptions
import mouse.all._
import seqexec.model.M1GuideConfig
import seqexec.model.M2GuideConfig
import seqexec.model.TelescopeGuideConfig
import seqexec.model.enum.M1Source
import seqexec.model.enum.NodAndShuffleStage
import seqexec.model.enum.TipTiltSource
import seqexec.server.ConfigResult
import seqexec.server.System
import seqexec.server.tcs.TcsController._

trait Tcs[F[_]] extends System[F] {
  def nod(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[ConfigResult[F]]
}

object Tcs {

  val defaultGuiderConf = GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
  def calcGuiderConfig(inUse: Boolean, guideWith: Option[StandardGuideOptions.Value]): GuiderConfig =
    guideWith.flatMap(v => inUse.option(GuiderConfig(v.toProbeTracking, v.toGuideSensorOption)))
      .getOrElse(defaultGuiderConf)

  // Shouldn't these be defined somewhere ?
  val GUIDE_WITH_PWFS1_PROP: String = "guideWithPWFS1"
  val GUIDE_WITH_PWFS2_PROP: String = "guideWithPWFS2"
  val GUIDE_WITH_AOWFS_PROP: String = "guideWithAOWFS"
  val P_OFFSET_PROP: String = "p"
  val Q_OFFSET_PROP: String = "q"

  // Conversions from ODB model values to TCS configuration values
  implicit class GuideWithOps(guideWith: StandardGuideOptions.Value) {
    val toProbeTracking: ProbeTrackingConfig = guideWith match {
      case StandardGuideOptions.Value.park => ProbeTrackingConfig.Parked
      case StandardGuideOptions.Value.freeze => ProbeTrackingConfig.Frozen
      case StandardGuideOptions.Value.guide => ProbeTrackingConfig.On(NodChopTrackingConfig.Normal)
    }

    val toGuideSensorOption: GuiderSensorOption = {
      if (guideWith.isActive) GuiderSensorOn
      else GuiderSensorOff
    }
  }

  def calcGuiderInUse(telGuide: TelescopeGuideConfig, tipTiltSource: TipTiltSource, m1Source: M1Source): Boolean = {
    val usedByM1: Boolean = telGuide.m1Guide match {
      case M1GuideConfig.M1GuideOn(src) => src === m1Source
      case _                            => false
    }
    val usedByM2 = telGuide.m2Guide match {
      case M2GuideConfig.M2GuideOn(_, srcs) => srcs.contains(tipTiltSource)
      case _                                => false
    }

    usedByM1 | usedByM2
  }

}
