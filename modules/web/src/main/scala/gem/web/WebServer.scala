// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import cats.effect._
import gem.dao.DatabaseConfiguration
import org.http4s._
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.syntax.kleisli._

object WebServer {

  // Single-element stream that creates and yields a web server, guaranteeing cleanup.
  private def server[F[_]: ConcurrentEffect: Timer](cfg: WebConfiguration.WebServer, root: HttpRoutes[F]): Resource[F, Server[F]] =
    BlazeServerBuilder[F]
      .bindHttp(cfg.port, cfg.host)
      .withHttpApp(root.orNotFound).resource

  /** Resource that creates and yields a web server, guaranteeing cleanup. */
  def resource[F[_]: ConcurrentEffect: ContextShift: Timer](web: WebConfiguration, db: DatabaseConfiguration)(
    implicit ev: ContextShift[IO]
  ): Resource[F, Server[F]] =
    for {
      env <- Environment.resource[F](web, db)
      g    = Gatekeeper[F](env)
      s    = Application.service[F]
      svr <- server[F](env.config.webServer, g(s))
    } yield svr

}
