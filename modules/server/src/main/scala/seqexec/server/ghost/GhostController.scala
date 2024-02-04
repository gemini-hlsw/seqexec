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
import fs2.Stream
import squants.time.Time
import squants.time.Seconds

trait GhostController[F[_]] extends GiapiInstrumentController[F, GhostConfig] {
  def gdsClient: GdsClient[F]

  def exposureProgress: F[Stream[F, Int]]

  def redDetectorActivity: F[Int]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def pauseObserve: F[Unit]

  def resumePaused(expTime: Time): F[ObserveCommandResult]

  def stopPaused: F[ObserveCommandResult]

  def abortPaused: F[ObserveCommandResult]

  def dcIsPreparing: F[Boolean]

  def dcIsAcquiring: F[Boolean]

  def dcIsReadingOut: F[Boolean]

}

object GhostController {
  def apply[F[_]: Sync: Logger](client: GhostClient[F], gds: GdsClient[F]): GhostController[F] =
    new AbstractGiapiInstrumentController[F, GhostConfig, GhostClient[F]](client, Seconds(90))
      with GhostController[F] {

      override val gdsClient: GdsClient[F] = gds

      override val name = "GHOST"

      override def configuration(config: GhostConfig): F[Configuration] =
        Logger[F].debug(pprint.apply(config.configuration).toString) *> config.configuration.pure[F]

      override def exposureProgress: F[Stream[F, Int]] =
        client.redProgress

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

      override def redDetectorActivity: F[Int] =
        client.redDetectorActivity.flatTap(x => Logger[F].debug(s"Red detector activity: $x"))

      // Numbers are defined on the GHOST ICD
      def dcIsPreparing: F[Boolean] = redDetectorActivity.map(x => x < 3 || x > 4)

      def dcIsAcquiring: F[Boolean] = redDetectorActivity.map(_ === 3)

      def dcIsReadingOut: F[Boolean] = redDetectorActivity.map(_ === 4)
    }
}
