package gem.dao

import gem.config.{GcalConfig, SmartGcalKey, F2SmartGcalKey}
import gem.enum.{F2Disperser, F2Filter, F2FpUnit, SmartGcalType}

import doobie.imports._

import java.time.{Duration, Instant}

import scala.util.Random
import scalaz._, Scalaz._
import scalaz.effect.IO

// Sample code that exercises SmartGcalDao.select.

object SmartGcalSample {
  val xa = DriverManagerTransactor[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  val allF2: Vector[SmartGcalKey] =
    (for {
      u <- F2FpUnit.all
      f <- F2Filter.all
      d <- F2Disperser.all
    } yield F2SmartGcalKey(u, f, d)).toVector


  val rand = new Random(0)

  def nextType(): SmartGcalType =
    SmartGcalType.all(rand.nextInt(4))

  def nextKey(): SmartGcalKey =
    allF2(rand.nextInt(allF2.size))

  def runSelects: ConnectionIO[List[(SmartGcalKey, SmartGcalType, List[GcalConfig])]] =
    (1 to 1000).toList.map(_ => (nextKey, nextType)).traverseU { case (k, t) =>
      SmartGcalDao.select(k, t).map { g => (k, t, g) }
    }

  def runc: IO[Unit] =
    for {
      l <- runSelects.transact(xa)
      _ <- IO.putStrLn(l.mkString(",\n"))
      _ <- IO.putStrLn("Done.")
    } yield ()

  def main(args: Array[String]): Unit = {
    val start = Instant.now()
    runc.unsafePerformIO()
    val end   = Instant.now()

    println(Duration.ofMillis(end.toEpochMilli - start.toEpochMilli))
    // A bit less than 1.6 seconds ...
  }
}
