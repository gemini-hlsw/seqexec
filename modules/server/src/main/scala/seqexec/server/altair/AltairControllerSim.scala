// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Applicative
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import seqexec.model.`enum`.Instrument
import seqexec.server.altair.AltairController.AltairPauseResume
import seqexec.server.tcs.Gaos.GuideCapabilities
import seqexec.server.tcs.Gaos.PauseConditionSet
import seqexec.server.tcs.Gaos.ResumeConditionSet
import seqexec.server.tcs.TcsController
import squants.Time

object AltairControllerSim {
  def apply[F[_]: Applicative: Logger]: AltairController[F] = new AltairController[F] {
    private val L = Logger[F]

    override def pauseResume(
      pauseReasons:  PauseConditionSet,
      resumeReasons: ResumeConditionSet,
      currentOffset: TcsController.FocalPlaneOffset,
      instrument:    Instrument
    )(cfg: AltairController.AltairConfig): F[AltairPauseResume[F]] =
      AltairPauseResume(
        L.info(s"Simulate pausing Altair loops because of $pauseReasons").some,
        GuideCapabilities(canGuideM2 = false, canGuideM1 = false),
        pauseTargetFilter = false,
        L.info(s"Simulate restoring Altair configuration $cfg because of $resumeReasons").some,
        GuideCapabilities(canGuideM2 = false, canGuideM1 = false),
        none,
        forceFreeze = true
      ).pure[F]

    override def observe(expTime: Time)(cfg: AltairController.AltairConfig): F[Unit] =
      L.info("Simulate observe notification for Altair")

    override def endObserve(cfg: AltairController.AltairConfig): F[Unit] =
      L.info("Simulate endObserve notification for Altair")

    override def isFollowing: F[Boolean] = false.pure[F]
  }
}
