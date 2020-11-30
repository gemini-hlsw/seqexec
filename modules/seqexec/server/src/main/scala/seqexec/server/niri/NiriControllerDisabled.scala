package seqexec.server.niri

import cats.Applicative
import cats.implicits._
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.model.`enum`.ObserveCommandResult
import seqexec.model.dhs.ImageFileId
import seqexec.server.Progress
import seqexec.server.SystemOverrides.overrideLogMessage
import squants.Time
import squants.time.TimeConversions._

class NiriControllerDisabled[F[_]: Logger: Applicative] extends NiriController[F] {
  private val name = "NIRI"

  override def applyConfig(config: NiriController.NiriConfig): F[Unit] = overrideLogMessage(name, "applyConfig")

  override def observe(fileId: ImageFileId, cfg: NiriController.DCConfig): F[ObserveCommandResult] =
    overrideLogMessage(name, s"observe $fileId").as(ObserveCommandResult.Success)

  override def endObserve: F[Unit] = overrideLogMessage(name, "endObserve")

  override def stopObserve: F[Unit] = overrideLogMessage(name, "stopObserve")

  override def abortObserve: F[Unit] = overrideLogMessage(name, "abortObserve")

  override def observeProgress(total: Time): Stream[F, Progress] = Stream.empty

  override def calcTotalExposureTime(cfg: NiriController.DCConfig): F[Time] = {
    val MinIntTime = 0.5.seconds

    (cfg.exposureTime + MinIntTime) * cfg.coadds.toDouble
  }.pure[F]
}
