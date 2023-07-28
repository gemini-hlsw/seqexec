// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.effect.Sync
import cats.syntax.all._
import giapi.client.commands.Configuration
import giapi.client.ghost.GhostClient
import org.typelevel.log4cats.Logger
import seqexec.server.AbstractGiapiInstrumentController
import seqexec.server.GiapiInstrumentController
import seqexec.server.keywords.GdsClient
import seqexec.model.enum.ObserveCommandResult
import squants.time.Time

trait GhostController[F[_]] extends GiapiInstrumentController[F, GhostConfig] {
  def gdsClient: GdsClient[F]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def pauseObserve: F[Unit]

  def resumePaused(expTime: Time): F[ObserveCommandResult]

  def stopPaused: F[ObserveCommandResult]

  def abortPaused: F[ObserveCommandResult]
}

object GhostController {
  def apply[F[_]: Sync: Logger](client: GhostClient[F], gds: GdsClient[F]): GhostController[F] =
    new AbstractGiapiInstrumentController[F, GhostConfig, GhostClient[F]](client)
      with GhostController[F] {

      override val gdsClient: GdsClient[F] = gds

      override val name = "GHOST"

      override def configuration(config: GhostConfig): F[Configuration] = {
        val c = config.configuration
        pprint.pprintln(c.config.toList.sortBy(_._1))
        c.pure[F]
      }

      override def stopObserve: F[Unit] =
        client.stop.void

      override def abortObserve: F[Unit] =
        client.abort.void

      override def pauseObserve: F[Unit] =
        client.pause.void

      override def resumePaused(expTime: Time): F[ObserveCommandResult] =
        client.continue.map(_ => ObserveCommandResult.Success)

      override def stopPaused: F[ObserveCommandResult] =
        client.stop.map(_ => ObserveCommandResult.Stopped)

      override def abortPaused: F[ObserveCommandResult] =
        client.abort.map(_ => ObserveCommandResult.Aborted)

    }
}
