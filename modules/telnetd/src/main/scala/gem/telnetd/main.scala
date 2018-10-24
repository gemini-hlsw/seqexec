// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd

import cats.effect.IO
import gem.dao.DatabaseConfiguration
import fs2.{ Stream, StreamApp }

object Main extends StreamApp[IO] {
  import StreamApp.ExitCode

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    for {
      _ <- TelnetServer.stream(DatabaseConfiguration.forTesting, TelnetdConfiguration.forTesting)
      _ <- Stream.eval(IO(Console.println("Press a key to exit."))) // scalastyle:off
      _ <- Stream.eval(IO(scala.io.StdIn.readLine()))
      _ <- Stream.eval(requestShutdown)
    } yield ExitCode(0)

}
