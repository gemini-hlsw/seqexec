// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import cats.effect._
import gem.dao.DatabaseConfiguration

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    WebServer.resource[IO](WebConfiguration.forTesting, DatabaseConfiguration.forTesting).use { _ =>
      for {
        _ <- IO(Console.println("Press a key to exit."))
        _ <- IO(scala.io.StdIn.readLine())
      } yield ExitCode.Success
    }

}
