// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.ghost

import cats.effect._
import cats.implicits._
import giapi.client.Giapi
import giapi.client.GiapiClient
import giapi.client.syntax.giapiconfig._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

/** Client for GHOST */
final class GhostClient[F[_]](override val giapi: Giapi[F])
    extends GiapiClient[F]

object GhostClient {
  // Used for simulations
  def simulatedGhostClient(
    ec: ExecutionContext): Resource[IO, GhostClient[IO]] =
    Resource.liftF(
      Giapi.giapiConnectionIO(ec).connect.map(new GhostClient(_))
    )

  def ghostClient[F[_]: ConcurrentEffect](
    url:     String,
    context: ExecutionContext): Resource[F, GhostClient[F]] = {
    val ghostStatus: Resource[F, Giapi[F]] =
      Resource.make(
        Giapi
          .giapiConnection[F](
            url,
            context
          )
          .connect)(_.close)

    val ghostSequence: Resource[F, Giapi[F]] =
      Resource.make(
        Giapi
          .giapiConnection[F](
            url,
            context
          )
          .connect)(_.close)

    for {
      _ <- ghostStatus
      c <- ghostSequence
    } yield new GhostClient(c)
  }
}

object GhostExample extends IOApp {

  val url = "failover:(tcp://127.0.0.1:61616)"

  val ghostClient: Resource[IO, GhostClient[IO]] =
    GhostClient.ghostClient(url, ExecutionContext.global)

  def run(args: List[String]): IO[ExitCode] =
    ghostClient.use { client =>
      for {
        r <- client.observe("TEST_S20180509", 5.seconds)
        _ <- IO(println(r)) // scalastyle:off console.io
      } yield ExitCode.Success
    }

}
