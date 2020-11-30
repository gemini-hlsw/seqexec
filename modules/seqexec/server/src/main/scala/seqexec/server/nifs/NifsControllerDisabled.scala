package seqexec.server.nifs

import cats.Applicative
import cats.implicits._
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.model.`enum`.ObserveCommandResult
import seqexec.model.dhs.ImageFileId
import seqexec.server.Progress
import seqexec.server.SystemOverrides.overrideLogMessage
import squants.Time

class NifsControllerDisabled[F[_]: Logger: Applicative] extends NifsController[F] {
  private val name = "NIFS"

  override def applyConfig(config: NifsController.NifsConfig): F[Unit] = overrideLogMessage(name, "applyConfig")

  override def observe(fileId: ImageFileId, cfg: NifsController.DCConfig): F[ObserveCommandResult] =
    overrideLogMessage(name, "").as(ObserveCommandResult.Success)

  override def endObserve: F[Unit] = overrideLogMessage(name, "endObserve")

  override def stopObserve: F[Unit] = overrideLogMessage(name, "stopObserve")

  override def abortObserve: F[Unit] = overrideLogMessage(name, "abortObserve")

  override def observeProgress(total: Time): Stream[F, Progress] = Stream.empty

  override def calcTotalExposureTime(cfg: NifsController.DCConfig): Time = NifsController.calcTotalExposureTime[F](cfg)
}
