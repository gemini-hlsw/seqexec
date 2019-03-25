// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd

import cats.effect._
import gem.dao.DatabaseConfiguration

object Main extends IOApp {

  def server: Resource[IO, Unit] =
    TelnetServer.server[IO](DatabaseConfiguration.forTesting, TelnetdConfiguration.forTesting)

  override def run(args: List[String]): IO[ExitCode] =
    server.use { _ =>
      for {
        _ <- IO(Console.println("Press a key to exit.")) // scalastyle:ignore
        _ <- IO(scala.io.StdIn.readLine())
      } yield ExitCode.Success
    }

}
