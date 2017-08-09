// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import gem.arb.ArbEnumerated._
import gem.config.GcalConfig
import gem.config.DynamicConfig
import gem.config.DynamicConfig.SmartGcalSearchKey
import gem.enum.SmartGcalType

import doobie.imports._

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

import cats._, cats.data._, cats.implicits._

// Sample code that exercises SmartGcalDao.select.
object SmartGcalSample extends TimedSample with gem.config.Arbitraries {

  type Result = List[(SmartGcalSearchKey, SmartGcalType, List[GcalConfig])]

  private def nextType(): Option[SmartGcalType] =
    arbitrary[SmartGcalType].sample

  private def nextDynamicConfig(): Option[DynamicConfig] =
    Gen.oneOf(
      arbitrary[DynamicConfig.F2       ],
      arbitrary[DynamicConfig.GmosNorth]
    ).sample

  private def nextKey(): Option[SmartGcalSearchKey] =
    nextDynamicConfig.flatMap(_.smartGcalKey)

  private def nextTest(): Option[(SmartGcalSearchKey, SmartGcalType)] =
    for {
      k <- nextKey()
      t <- nextType()
    } yield (k, t)

  override def runl(args: List[String]): ConnectionIO[Result] =
    ((1 to 1000).toList.as(nextTest().toList).flatten).traverseU { case (k, t) =>
      SmartGcalDao.select(k, t).map { g => (k, t, g) }
    }

  override def format(r: Result): String =
    r.mkString(", \n")
}
