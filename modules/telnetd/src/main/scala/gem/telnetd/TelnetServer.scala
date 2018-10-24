// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.telnetd

import gem.Log
import cats.effect.{ IO }
import gem.dao.DatabaseConfiguration
import fs2.Stream
import tuco._, Tuco._

object TelnetServer {

  // Single-element stream starting a server, yielding unit, automatically cleaned up.
  private def server(db: DatabaseConfiguration, telnet: TelnetdConfiguration, log: Log[SessionIO]): Stream[IO, Unit] = {
    Stream.bracket(
      Config(Interaction.main(
        db.transactor[SessionIO], log
      ), telnet.port).start
    )(_ => Stream(()), identity)
  }

  /** Single-element stream starting a server, yielding unit, automatically cleaned up. */
  def stream(db: DatabaseConfiguration, telnet: TelnetdConfiguration): Stream[IO, Unit] =
    Stream.eval(Log.newLogIn[SessionIO, IO]("telnetd", db.transactor[IO])).flatMap { log =>
      server(db, telnet, log)
    }

}