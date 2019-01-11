// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.ghost

import cats.effect._
import cats.implicits._
import giapi.client.{Giapi, GiapiClient}
import giapi.client.syntax.giapiconfig._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

/** Client for GHOST */
final class GhostClient[F[_]](override val giapi: Giapi[F]) extends GiapiClient[F]

object GhostExample extends IOApp {

  val ghostStatus: Resource[IO, Giapi[IO]] =
    Resource.make(
      Giapi.giapiConnection[IO](
        "failover:(tcp://127.0.0.1:61616)",
        ExecutionContext.global
      ).connect)(_.close)

  val ghostSequence: Resource[IO, Giapi[IO]] =
    Resource.make(
      Giapi.giapiConnection[IO](
        "failover:(tcp://127.0.0.1:61616)",
        ExecutionContext.global
      ).connect)(_.close)

  val ghostClient: Resource[IO, GhostClient[IO]] =
    for {
      _ <- ghostStatus
      c <- ghostSequence
    } yield new GhostClient(c)

  def run(args: List[String]): IO[ExitCode] =
    ghostClient.use { client =>
      for {
        r <- client.observe("TEST_S20180509", 5.seconds)
        _ <- IO(println(r)) // scalastyle:off console.io
      } yield ExitCode.Success
    }

}
