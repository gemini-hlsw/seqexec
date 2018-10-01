package giapi.client.ghost

import cats.implicits._
import fs2.Stream
import giapi.client.{Giapi, GiapiClient}

import scala.concurrent.duration._

/**
  * Client for GHOST
  */
class GHOSTClient[F[_]](override val giapi: Giapi[F]) extends GiapiClient[F] {
  // TODO: Should this be moved to the GIAPIClient? Or will GHOST have a specific value?
  val DefaultCommandTimeout: FiniteDuration = 60.seconds
}

object GHOSTExample extends App {

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
          new GHOSTClient[IO](giapi)
        val r = for {
          f <- client.observe("TEST_S20180509", 5.seconds)
        } yield f
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  (ghostStatus ++ ghostSequence).compile.drain.unsafeRunSync()
}
