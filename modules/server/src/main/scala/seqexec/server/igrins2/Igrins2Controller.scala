// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.igrins2

import seqexec.server.GiapiInstrumentController
import seqexec.server.keywords.GdsClient
import giapi.client.igrins2.Igrins2Client
import seqexec.server.AbstractGiapiInstrumentController
import cats.effect.Sync
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import giapi.client.commands.Configuration

trait Igrins2Config {
  def configuration: Configuration
}

trait Igrins2Controller[F[_]] extends GiapiInstrumentController[F, Igrins2Config] {
  def gdsClient: GdsClient[F]

}

object Igrins2Controller {
  def apply[F[_]: Sync: Logger](client: Igrins2Client[F], gds: GdsClient[F]): Igrins2Controller[F] =
    new AbstractGiapiInstrumentController[F, Igrins2Config, Igrins2Client[F]](client)
      with Igrins2Controller[F] {
      override val gdsClient: GdsClient[F] = gds

      override val name = "IGRINS-2"

      override def configuration(config: Igrins2Config): F[Configuration] = {
        pprint.pprintln(config.configuration)
        config.configuration.pure[F]
      }
    }
}
