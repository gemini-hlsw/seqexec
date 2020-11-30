package seqexec.server.altair

import cats.Applicative
import io.chrisdavenport.log4cats.Logger
import cats.implicits._
import seqexec.server.SystemOverrides.overrideLogMessage
import seqexec.server.altair.AltairController.FieldLens
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.PauseResume
import squants.Time

class AltairControllerDisabled[F[_]: Logger: Applicative] extends AltairController[F] {
  override def pauseResume(pauseReasons: Gaos.PauseConditionSet,
                           resumeReasons: Gaos.ResumeConditionSet,
                           fieldLens: FieldLens
                          )(cfg: AltairController.AltairConfig): F[Gaos.PauseResume[F]] =
    PauseResume(
      overrideLogMessage("Altair", "pause AO loops").some,
      overrideLogMessage("Altair", "resume AO loops").some
    ).pure[F]

  override def observe(expTime: Time)(cfg: AltairController.AltairConfig): F[Unit] =
    overrideLogMessage("Altair", "observe")

  override def endObserve(cfg: AltairController.AltairConfig): F[Unit] =
    overrideLogMessage("Altair", "endObserve")

  override def isFollowing: F[Boolean] = false.pure[F]
}
