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
import squants.time.Seconds

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
    new AbstractGiapiInstrumentController[F, GhostConfig, GhostClient[F]](client, Seconds(90))
      with GhostController[F] {

      override val gdsClient: GdsClient[F] = gds

      override val name = "GHOST"

      private def isAGIdle: F[Boolean] =
        // 2 is idle, and 1 is guiding. We have more intermediate states but we'll
        // assume we don't want to move focus
        client.guidingState.map(_.exists(_ === 2))

      override def configuration(config: GhostConfig): F[Configuration] =
        for {
          baseConfig <- config.configuration.pure[F]
          value      <- client.guidingState
          idle       <- isAGIdle
          finalConfig = baseConfig |+| config.moveIFUToFocus.when(_ => idle)
          _          <- Logger[F].debug(s"Guiding check with value: $value isGuiding off: $idle")
          _          <- Logger[F].debug(pprint.apply(finalConfig).toString)
        } yield finalConfig

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
