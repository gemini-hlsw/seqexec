// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.ghost

import fs2.Stream
import giapi.client.{Giapi, GiapiClient}
import giapi.client.GiapiConfig._

/**
  * Client for GHOST
  */
final class GhostClient[F[_]](override val giapi: Giapi[F]) extends GiapiClient[F]

object GhostExample extends App {

  import cats.effect.IO
  import scala.concurrent.duration._

  private val ghostStatus =
    Stream.bracket(
      Giapi
        .giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)",
        scala.concurrent.ExecutionContext.Implicits.global)
        .connect)(
      _ => {
        val r: IO[Unit] = IO.unit
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  private val ghostSequence =
    Stream.bracket(
      Giapi
        .giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)",
                              scala.concurrent.ExecutionContext.Implicits.global)
        .connect)(
      giapi => {
        val client =
          new GhostClient[IO](giapi)
        val r = for {
          f <- client.observe("TEST_S20180509", 5.seconds)
        } yield f
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  (ghostStatus ++ ghostSequence).compile.drain.unsafeRunSync()
}
