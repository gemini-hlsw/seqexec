// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Applicative
import cats.implicits._
import org.typelevel.log4cats.Logger
import seqexec.model.`enum`.Instrument
import seqexec.server.overrideLogMessage
import seqexec.server.altair.AltairController.AltairPauseResume
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.GuideCapabilities
import seqexec.server.tcs.TcsController.FocalPlaneOffset
import squants.Time

class AltairControllerDisabled[F[_]: Logger: Applicative] extends AltairController[F] {
  override def pauseResume(
    pauseReasons:  Gaos.PauseConditionSet,
    resumeReasons: Gaos.ResumeConditionSet,
    currentOffset: FocalPlaneOffset,
    instrument:    Instrument
  )(cfg: AltairController.AltairConfig): F[AltairPauseResume[F]] =
    AltairPauseResume(
      overrideLogMessage("Altair", "pause AO loops").some,
      GuideCapabilities(canGuideM2 = false, canGuideM1 = false),
      pauseTargetFilter = false,
      overrideLogMessage("Altair", "resume AO loops").some,
      GuideCapabilities(canGuideM2 = false, canGuideM1 = false),
      none,
      forceFreeze = true
    ).pure[F]

  override def observe(expTime: Time)(cfg: AltairController.AltairConfig): F[Unit] =
    overrideLogMessage("Altair", "observe")

  override def endObserve(cfg: AltairController.AltairConfig): F[Unit] =
    overrideLogMessage("Altair", "endObserve")

  override def isFollowing: F[Boolean] = false.pure[F]

}
