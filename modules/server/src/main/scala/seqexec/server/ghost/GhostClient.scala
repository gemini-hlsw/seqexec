// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.ghost

import cats.effect.Temporal
import cats.effect._
import cats.syntax.all._
import giapi.client.Giapi
import giapi.client.GiapiClient
import giapi.client.GiapiStatusDb
import giapi.client.syntax.status._
import fs2.Stream

import scala.concurrent.duration._
import cats.Functor

/** Client for GHOST */
sealed trait GhostClient[F[_]] extends GiapiClient[F] {
  def redProgress: F[Stream[F, Int]]

  def redDetectorActivity: F[Int]
}

object GhostClient {
  val RedProgress: String      = "ghost:sad:dc:red.progress"
  val RedDetectorState: String = "ghost:sad:dc:red.activity"
  val prefixes                 = List("ghost:sad:dc:red.*", "ghost:sad:dc:blue.*")

  private final class GhostClientImpl[F[_]: Functor](
    override val giapi: Giapi[F],
    val statusDb:       GiapiStatusDb[F]
  ) extends GhostClient[F] {
    def redProgress: F[Stream[F, Int]] =
      giapi.stream[Int](RedProgress)

    def redDetectorActivity: F[Int] =
      statusDb.optional(RedDetectorState).map(_.intValue.getOrElse(0))
  }

  // Used for simulations
  def simulatedGhostClient[F[_]: Temporal]: Resource[F, GhostClient[F]] =
    Resource.eval(
      Giapi
        .simulatedGiapiConnection[F]
        .connect
        .map(new GhostClientImpl(_, GiapiStatusDb.simulatedDb[F]))
    )

  def ghostClient[F[_]: Async](
    url:               String,
    statusesToMonitor: List[String]
  ): Resource[F, GhostClient[F]] = {
    val ghostClient: Resource[F, Giapi[F]] =
      Resource.make(Giapi.giapiConnection[F](url, prefixes).connect)(_.close)

    val statusDb: Resource[F, GiapiStatusDb[F]] =
      Resource.make(
        GiapiStatusDb
          .newStatusDb[F](url, List(RedProgress, RedDetectorState) ++ statusesToMonitor, prefixes)
      )(_.close)

    for {
      cl <- ghostClient
      db <- statusDb
    } yield new GhostClientImpl(cl, db)
  }
}

object GhostExample extends IOApp {

  val url = "failover:(tcp://127.0.0.1:61616)"

  val ghostClient: Resource[IO, GhostClient[IO]] =
    GhostClient.ghostClient(url, Nil)

  def run(args: List[String]): IO[ExitCode] =
    ghostClient.use { client =>
      for {
        r <- client.observe("TEST_S20180509", 5.seconds)
        _ <- IO(println(r)) // scalastyle:off console.io
      } yield ExitCode.Success
    }

}
