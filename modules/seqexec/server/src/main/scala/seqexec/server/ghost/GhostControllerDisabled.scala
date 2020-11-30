package seqexec.server.ghost

import cats.Applicative
import cats.implicits._
import gem.Observation
import io.chrisdavenport.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.server.SystemOverrides.overrideLogMessage
import seqexec.server.keywords.{GdsClient, KeywordBag}
import squants.time.Time

class GhostControllerDisabled[F[_]: Logger: Applicative] extends GhostController[F] {
  private val name = "GHOST"

  override def gdsClient: GdsClient[F] = new GdsClient[F] {
    override def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit] = overrideLogMessage(name, "setKeywords")

    override def openObservation(obsId: Observation.Id, id: ImageFileId, ks: KeywordBag): F[Unit] =
      overrideLogMessage(name, "openObservation")

    override def closeObservation(id: ImageFileId): F[Unit] = overrideLogMessage(name, "closeObservation")
  }

  override def applyConfig(config: GhostConfig): F[Unit] = overrideLogMessage(name, "applyConfig")

  override def observe(fileId: ImageFileId, expTime: Time): F[ImageFileId] = overrideLogMessage(name, s"observe $fileId").as(fileId)

  override def endObserve: F[Unit] = overrideLogMessage(name, "endObserve")
}
