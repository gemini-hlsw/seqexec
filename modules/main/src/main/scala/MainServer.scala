// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.main

import cats.effect.IO
import gem.dao.DatabaseConfiguration
import gem.web.WebServer
import gem.telnetd.TelnetServer
import fs2.Stream
import org.flywaydb.core.Flyway

object MainServer {

  // Run flyway migrations
  private def migrate(db: DatabaseConfiguration): IO[Int] =
    IO {
      val flyway = new Flyway()
      flyway.setDataSource(db.connectUrl, db.userName, db.password);
      flyway.migrate()
    }

  /** A single-element stream that starts the server up and shuts it down on exit. */
  def stream(cfg: MainConfiguration): Stream[IO, Unit] =
    for {
      _   <- Stream.eval(migrate(cfg.database))
      _   <- TelnetServer.stream(cfg.database, cfg.telnetd)
      _   <- WebServer.stream(cfg.web, cfg.database)
    } yield ()

}