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

  def runSelects(qs: List[(SmartGcalKey, SmartGcalType)]): ConnectionIO[List[(SmartGcalKey, SmartGcalType, List[GcalConfig])]] =
    qs.traverseU { case (k, t) =>
      SmartGcalDao.select(k, t).map { g => (k, t, g) }
    }

  def main(args: Array[String]): Unit = {
    val querys = (1 to 1000).toList.map(_ => (nextKey, nextType))
    val start  = Instant.now()
    val result = runSelects(querys).transact(xa).unsafePerformIO()
    val end    = Instant.now()

    println(result.mkString(", \n"))
    println(Duration.ofMillis(end.toEpochMilli - start.toEpochMilli))

    //
    // A bit less than 1.6 seconds ...  adding indices doesn't seem to make a
    // difference:
    //
    //    "smart_f2_baseline_disperser_filter_fpu_idx" btree (baseline, disperser, filter, fpu)
    //    "smart_f2_lamp_disperser_filter_fpu_idx" btree (lamp, disperser, filter, fpu)
    //

  }
}
