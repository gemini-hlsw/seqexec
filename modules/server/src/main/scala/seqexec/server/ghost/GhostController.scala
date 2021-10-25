// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
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

trait GhostController[F[_]] extends GiapiInstrumentController[F, GhostConfig] {
  def gdsClient: GdsClient[F]
}
object GhostController {
  def apply[F[_]: Sync: Logger](client: GhostClient[F], gds: GdsClient[F]): GhostController[F] =
    new AbstractGiapiInstrumentController[F, GhostConfig, GhostClient[F]](client)
      with GhostController[F] {
      override val gdsClient: GdsClient[F] = gds

      override val name = "GHOST"

      override def configuration(config: GhostConfig): F[Configuration] =
        Logger[F].info(s"ABC ${config.configuration}") *> config.configuration.pure[F]
    }
}
