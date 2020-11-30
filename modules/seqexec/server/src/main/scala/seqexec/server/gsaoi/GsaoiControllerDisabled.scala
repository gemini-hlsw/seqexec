package seqexec.server.gsaoi

import cats.Functor
import cats.implicits._
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.model.`enum`.ObserveCommandResult
import seqexec.model.dhs.ImageFileId
import seqexec.server.Progress
import seqexec.server.SystemOverrides.overrideLogMessage
import squants.Time

class GsaoiControllerDisabled[F[_]: Logger: Functor] extends GsaoiController[F] {
  private val name = "GSAOI"

  override def applyConfig(config: GsaoiController.GsaoiConfig): F[Unit] = overrideLogMessage(name, "")

  override def observe(fileId: ImageFileId, cfg: GsaoiController.DCConfig): F[ObserveCommandResult] =
    overrideLogMessage(name, s"observe $fileId").as(ObserveCommandResult.Success)

  override def endObserve: F[Unit] = overrideLogMessage(name, "endObserve")

  override def stopObserve: F[Unit] = overrideLogMessage(name, "stopObserve")

  override def abortObserve: F[Unit] = overrideLogMessage(name, "abortObserve")

  override def observeProgress(total: Time): Stream[F, Progress] = Stream.empty
}
