// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import doobie.imports._
import gem._
import EventLogDao._

import java.time.Instant

import scalaz.effect.{IO, SafeApp}

object EventLogDaoSample extends SafeApp {

  val xa: Transactor[IO] =
    Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:gem",
      "postgres",
      ""
    )

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

  override def runl(args: List[String]): IO[Unit] =
    for {
      l <- insertEvents.transact(xa)
      _ <- IO.putStrLn(l.mkString(",\n"))
      _ <- IO.putStrLn("Done.")
    } yield ()

}
