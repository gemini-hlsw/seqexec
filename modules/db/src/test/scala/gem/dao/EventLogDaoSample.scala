package gem.dao

import doobie.imports._
import gem._
import EventLogDao._

import java.time.Instant

import scalaz.effect.{IO, SafeApp}

object EventLogDaoSample extends SafeApp {
  val xa = DriverManagerTransactor[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  val sid = Sequence.Id.unsafeFromString("GS-2017A-Q-1-2-acq")

  def insertEvents: ConnectionIO[List[Event]] =
    for {
      _ <- insertStartSlew(sid)
      _ <- insertStartVisit(sid)
      _ <- insertStartIntegration(sid, 1)
      _ <- insertEndIntegration(sid, 1)
      _ <- insertPauseObserve(sid)
      _ <- insertAbortObserve(sid)
      _ <- insertEndVisit(sid)
      _ <- insertEndSlew(sid)
      l <- selectAll(Instant.now().minusSeconds(10), Instant.now().plusSeconds(10))
    } yield l

  override def runl(args: List[String]): IO[Unit] =
    for {
      l <- insertEvents.transact(xa)
      _ <- IO.putStrLn(l.mkString(",\n"))
      _ <- IO.putStrLn("Done.")
    } yield ()

}
