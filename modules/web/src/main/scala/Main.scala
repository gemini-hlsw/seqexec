// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import cats.effect.IO
import org.http4s.HttpService
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeBuilder

object Main {

  /** Create a new server with the given config, mounting the given root service. */
  def newServer(cfg: Configuration.WebServer, root: HttpService[IO]): IO[Server[IO]] =
    BlazeBuilder[IO]
      .bindHttp(cfg.port, cfg.host)
      .mountService(root, "/")
      .start

  /** Entry point. Run the server with a test config, until someone stops it. */
  def runc: IO[Unit] =
    for {
      env <- Environment.quicken(Configuration.forTesting)
      svr <- newServer(env.config.webServer, Gatekeeper(env)(Application.service))
      _   <- IO(Console.println("Press a key to exit.")) // scalastyle:off
      _   <- IO(scala.io.StdIn.readLine())
      _   <- svr.shutdown
      _   <- env.shutdown
    } yield ()

  def main(args: Array[String]): Unit =
    runc.unsafeRunSync()

}
