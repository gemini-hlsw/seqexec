// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.giapi

import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import giapi.client.GiapiClient
import giapi.client.commands.{CommandResult, CommandResultException, Configuration}
import org.log4s.getLogger

import scala.concurrent.duration._
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords.GDSClient
import seqexec.server.SeqActionF
import seqexec.server.SeqexecFailure.{Execution, SeqexecException}
import squants.time.Time

/**
  * Superclass for all GIAPI instrument controllers.
  */
abstract class GiapiInstrumentController[F[_]: Sync, CFG, C <: GiapiClient[F]] {
  private val Log = getLogger

  def client: C
  def gdsClient: GDSClient
  def name: String
  def configuration(config: CFG): Configuration

  private def configure(config: CFG): SeqActionF[F, CommandResult] = {
    EitherT(client.genericApply(configuration(config)).attempt)
      .leftMap {
        // The GMP sends these cryptic messages but we can do better
        case CommandResultException(_, "Message cannot be null") => Execution("Unhandled Apply command")
        case CommandResultException(_, m)                        => Execution(m)
        case f                                                   => SeqexecException(f)
      }
  }

  def applyConfig(config: CFG): SeqActionF[F, Unit] =
    for {
      _ <- SeqActionF.apply(Log.debug(s"Start $name configuration"))
      _ <- SeqActionF.apply(Log.debug(s"$name configuration $config"))
      _ <- configure(config)
      _ <- SeqActionF.apply(Log.debug(s"Completed $name configuration"))
    } yield ()

  def observe(fileId: ImageFileId, expTime: Time): SeqActionF[F, ImageFileId] =
    EitherT(client.observe(fileId, expTime.toMilliseconds.milliseconds).map(_ => fileId).attempt)
      .leftMap {
        case CommandResultException(_, "Message cannot be null") => Execution("Unhandled observe command")
        case CommandResultException(_, m)                        => Execution(m)
        case f                                                   => SeqexecException(f)
      }

  def endObserve: SeqActionF[F, Unit] =
    SeqActionF.void
}
