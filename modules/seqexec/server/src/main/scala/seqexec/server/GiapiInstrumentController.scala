// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import edu.gemini.aspen.giapi.commands.HandlerResponse
import giapi.client.GiapiClient
import giapi.client.commands.CommandResult
import giapi.client.commands.CommandResultException
import giapi.client.commands.Configuration
import org.log4s.getLogger
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqexecFailure.Execution
import seqexec.server.SeqexecFailure.SeqexecException
import squants.time.Time
import scala.concurrent.duration._

trait GiapiInstrumentController[F[_], CFG] {
  def applyConfig(config: CFG): F[Unit]
  def observe(fileId: ImageFileId, expTime: Time): F[ImageFileId]
  def endObserve: F[Unit]
}

/**
  * Superclass for all GIAPI instrument controllers.
  */
abstract class AbstractGiapiInstrumentController[F[_]: Sync, CFG, C <: GiapiClient[F]](client: C) extends GiapiInstrumentController[F, CFG] {
  private val Log = getLogger

  def name: String
  def configuration(config: CFG): F[Configuration]

  private def adaptGiapiError: PartialFunction[Throwable, SeqexecFailure] = {
    // The GMP sends these cryptic messages but we can do better
    case CommandResultException(_, "Message cannot be null") =>
      Execution("Unhandled Apply command")
    case CommandResultException(_, m) => Execution(m)
    case f                            => SeqexecException(f)
  }

  private def configure(config: CFG): F[CommandResult] = {
    val cfg: F[Configuration] = configuration(config)
    val isEmpty               = cfg.map(_.config.isEmpty)
    isEmpty.ifM((CommandResult(HandlerResponse.Response.ACCEPTED)
                  .pure[F]),
                cfg.flatMap(client.genericApply))
  }.adaptError(adaptGiapiError)

  override def applyConfig(config: CFG): F[Unit] =
    for {
      _ <- Sync[F].delay(Log.debug(s"Start $name configuration"))
      _ <- Sync[F].delay(Log.debug(s"$name configuration $config"))
      _ <- configure(config)
      _ <- Sync[F].delay(Log.debug(s"Completed $name configuration"))
    } yield ()

  override def observe(fileId: ImageFileId, expTime: Time): F[ImageFileId] =
    client
      .observe(fileId, expTime.toMilliseconds.milliseconds)
      .as(fileId)
      .adaptError(adaptGiapiError)

  override def endObserve: F[Unit] =
    Applicative[F].unit
}
