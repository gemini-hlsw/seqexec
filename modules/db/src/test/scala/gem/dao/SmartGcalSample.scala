// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem.arb._
import gem.config._
import gem.enum.{ Instrument, SmartGcalType }
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

// Sample code that exercises SmartGcalDao.select.
object SmartGcalSample extends TimedSample {

  import ArbEnumerated._
  import ArbDynamicConfig._
  import ArbStaticConfig._

  type Result = List[(SmartGcalSearchKey, SmartGcalType, List[GcalConfig])]

  private def nextType(): Option[SmartGcalType] =
    arbitrary[SmartGcalType].sample

  private def nextKey(): Option[SmartGcalSearchKey] =
    (for {
      i <- Gen.oneOf(Instrument.Flamingos2, Instrument.GmosN, Instrument.GmosS, Instrument.Gnirs)
      s <- genStaticConfigOf(i)
      d <- genDynamicConfigOf(i)
    } yield d.smartGcalKey(s)).sample.flatten

  private def nextTest(): Option[(SmartGcalSearchKey, SmartGcalType)] =
    for {
      k <- nextKey()
      t <- nextType()
    } yield (k, t)

  override def runl(args: List[String]): ConnectionIO[Result] =
    ((1 to 1000).toList.flatMap(_ => nextTest().toList)).traverse { case (k, t) =>
      SmartGcalDao.select(k, t).map { g => (k, t, g) }
    }

  override def format(r: Result): String =
    r.mkString(", \n")
}
