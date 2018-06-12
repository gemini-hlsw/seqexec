// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import cats.effect.IO
import gem.dao.DatabaseConfiguration
import fs2.Stream
import org.http4s.HttpService
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeBuilder

object WebServer {

  // Single-element stream that creates and yields a web server, guaranteeing cleanup.
  private def server(cfg: WebConfiguration.WebServer, root: HttpService[IO]): Stream[IO, Server[IO]] =
    Stream.bracket(
      BlazeBuilder[IO]
        .bindHttp(cfg.port, cfg.host)
        .mountService(root, "/")
        .start)(
      s => Stream(s),
      _.shutdown
    )

  /** A single-element stream that creates and yields a web server, guaranteeing cleanup. */
  def stream(web: WebConfiguration, db: DatabaseConfiguration): Stream[IO, Server[IO]] =
    Environment.stream(web, db).flatMap { env =>
      server(env.config.webServer, Gatekeeper(env)(Application.service))
    }

}
