package gem.dao

import gem.config.{GcalConfig, SmartGcalKey, F2SmartGcalKey}
import gem.enum.{F2Disperser, F2Filter, F2FpUnit, SmartGcalType}

import doobie.imports._

import scala.util.Random
import scalaz._, Scalaz._

// Sample code that exercises SmartGcalDao.select.
object SmartGcalSample extends TimedSample {
  type Result = List[(SmartGcalKey, SmartGcalType, List[GcalConfig])]

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

  override def runl(args: List[String]): ConnectionIO[Result] =
    ((1 to 1000).toList.map(_ => (nextKey, nextType))).traverseU { case (k, t) =>
      SmartGcalDao.select(k, t).map { g => (k, t, g) }
    }

  override def format(r: Result): String =
    r.mkString(", \n")

  //
  // A bit less than 1.6 seconds ...  adding indices doesn't seem to make a
  // difference:
  //
  //    "smart_f2_baseline_disperser_filter_fpu_idx" btree (baseline, disperser, filter, fpu)
  //    "smart_f2_lamp_disperser_filter_fpu_idx" btree (lamp, disperser, filter, fpu)
  //
}
