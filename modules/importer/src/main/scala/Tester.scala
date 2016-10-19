package gem

import doobie.imports._
import gem.dao._

import java.time.Instant
import scalaz.effect.{IO, SafeApp}


object Tester extends SafeApp {
  val xa = DriverManagerTransactor[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  val sid = Sequence.Id.unsafeFromString("GS-2017A-Q-1-2-acq")

  override def runl(args: List[String]): IO[Unit] =
    for {
      _ <- EventLogDao.insertStartSlew(sid).transact(xa)
      _ <- EventLogDao.insertStartVisit(sid).transact(xa)
      _ <- EventLogDao.insertStartIntegration(sid, 1).transact(xa)
      _ <- EventLogDao.insertEndIntegration(sid, 1).transact(xa)
      _ <- EventLogDao.insertPauseObserve(sid).transact(xa)
      _ <- EventLogDao.insertAbortObserve(sid).transact(xa)
      _ <- EventLogDao.insertEndVisit(sid).transact(xa)
      _ <- EventLogDao.insertEndSlew(sid).transact(xa)
      l <- EventLogDao.selectAll(Instant.now().minusSeconds(10), Instant.now().plusSeconds(10)).transact(xa)
      _ <- IO.putStrLn(l.mkString(",\n"))
      _ <- IO.putStrLn("Done.")
    } yield ()

}
