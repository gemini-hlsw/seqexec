// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.effect.{ IO, ContextShift }
import doobie._, doobie.implicits._
import gem._
import java.time.Instant

object EventLogDaoSample {
  import EventLogDao._

  private implicit val contextShift: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.global)

  val xa: Transactor[IO] =
    DatabaseConfiguration.forTesting.transactor[IO]

  val oid: Observation.Id = Observation.Id.unsafeFromString("GS-2017A-Q-1-2")

  def insertEvents: ConnectionIO[List[Event]] =
    for {
      _ <- insertStartSlew(oid)
      _ <- insertStartVisit(oid)
      _ <- insertStartIntegration(oid, 1)
      _ <- insertEndIntegration(oid, 1)
      _ <- insertPauseObserve(oid)
      _ <- insertAbortObserve(oid)
      _ <- insertEndVisit(oid)
      _ <- insertEndSlew(oid)
      l <- selectAll(Instant.now().minusSeconds(10), Instant.now().plusSeconds(10))
    } yield l

  val run: IO[Unit] =
    for {
      l <- insertEvents.transact(xa)
      _ <- IO(Console.println(l.mkString(",\n")))
      _ <- IO(Console.println("Done."))
    } yield ()

  def main(args: Array[String]): Unit =
    run.unsafeRunSync

}
