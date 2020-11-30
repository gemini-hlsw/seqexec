package seqexec.server.gnirs

import cats.Applicative
import cats.implicits._
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.model.`enum`.ObserveCommandResult
import seqexec.model.dhs.ImageFileId
import seqexec.server.Progress
import seqexec.server.SystemOverrides.overrideLogMessage
import squants.Time

class GnirsControllerDisabled[F[_]: Logger: Applicative] extends GnirsController[F] {
  private val name = "GNIRS"

  override def applyConfig(config: GnirsController.GnirsConfig): F[Unit] = overrideLogMessage(name, "applyConfig")

  override def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] =
    overrideLogMessage(name, s"observe $fileId").as(ObserveCommandResult.Success)

  override def endObserve: F[Unit] = overrideLogMessage(name, "endObserve")

  override def stopObserve: F[Unit] = overrideLogMessage(name, "stopObserve")

  override def abortObserve: F[Unit] = overrideLogMessage(name, "abortObserve")

  override def observeProgress(total: Time): Stream[F, Progress] = Stream.empty

  override def calcTotalExposureTime(cfg: GnirsController.DCConfig): F[Time] =
    GnirsController.calcTotalExposureTime[F](cfg)
}
