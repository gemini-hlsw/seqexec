// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.telnetd

import gem.Log
import cats.effect._
import cats.implicits._
import gem.dao.DatabaseConfiguration
import tuco._, Tuco._

object TelnetServer {

  private def config[F[_]: Async](db: DatabaseConfiguration, telnet: TelnetdConfiguration, log: Log[SessionIO]): Config[F] =
    Config[F](Interaction.main(db.transactor[SessionIO], log), telnet.port)

  /** Resource starting a server, yielding unit, automatically cleaned up. */
  def server[F[_]: Async](db: DatabaseConfiguration, telnet: TelnetdConfiguration)(
    implicit cs: ContextShift[IO]
  ): Resource[F, Unit] =
    for {
      log <- Resource.liftF(Log.newLogIn[SessionIO, F]("telnetd", db.transactor[IO]))
      _   <- Resource.make(config[F](db, telnet, log).start)(identity)
    } yield ()

}