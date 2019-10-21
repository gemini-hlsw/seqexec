// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Applicative
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import seqexec.server.altair.AltairController.FieldLens
import seqexec.server.tcs.Gaos.{PauseConditionSet, PauseResume, ResumeConditionSet}
import squants.Time

object AltairControllerSim {
  def apply[F[_]: Applicative: Logger]: AltairController[F] = new AltairController[F] {
    private val L = Logger[F]

    override def pauseResume(
      pauseReasons: PauseConditionSet,
      resumeReasons: ResumeConditionSet,
      fieldLens: FieldLens)(
    cfg: AltairController.AltairConfig
    ): F[PauseResume[F]] =
      PauseResume(
        L.info(s"Simulate pausing Altair loops because of $pauseReasons").some,
        L.info(s"Simulate restoring Altair configuration $cfg because of $resumeReasons").some
      ).pure[F]

    override def observe(expTime: Time)(cfg: AltairController.AltairConfig): F[Unit] =
      L.info("Simulate observe notification for Altair")

    override def endObserve(cfg: AltairController.AltairConfig): F[Unit] =
      L.info("Simulate endObserve notification for Altair")

    override def isFollowing: F[Boolean] = false.pure[F]
  }
}
