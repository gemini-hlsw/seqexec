// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import gem.config.GcalConfig
import gem.config.DynamicConfig.SmartGcalKey
import gem.enum.{F2Disperser, F2Filter, F2FpUnit, SmartGcalType}

import doobie.imports._

import scala.util.Random
import scalaz._, Scalaz._

// Sample code that exercises SmartGcalDao.select.
object SmartGcalSample extends TimedSample {

  type Result = List[(SmartGcalKey, SmartGcalType, List[GcalConfig])]

  val allF2: Vector[SmartGcalKey] =
    (for {
      d <- F2Disperser.all
      f <- F2Filter.all
      u <- F2FpUnit.all
    } yield SmartGcalKey.F2(d, f, u)).toVector

  val rand: Random = new Random(0)

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
