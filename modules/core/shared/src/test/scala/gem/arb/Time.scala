// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.util.InstantMicros

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import scala.collection.JavaConverters._
import java.time._
import java.time.temporal.ChronoUnit

// Arbitrary but resonable dates and times.
trait ArbTime {

  implicit val arbZoneId: Arbitrary[ZoneId] =
    Arbitrary {
      oneOf(ZoneId.getAvailableZoneIds.asScala.toSeq).map(ZoneId.of)
    }

  implicit val arbYear: Arbitrary[Year] =
    Arbitrary {
      choose(2000, 2020).map(Year.of(_))
    }

  implicit val arbLocalDate: Arbitrary[LocalDate] =
    Arbitrary {
      for {
        y <- arbitrary[Year]
        d <- choose(1, y.length)
      } yield LocalDate.ofYearDay(y.getValue, d)
    }

  implicit val arbLocalTime: Arbitrary[LocalTime] =
    Arbitrary {
      for {
        h <- choose(0, 23)
        m <- choose(0, 59)
        s <- choose(0, 59)
        n <- choose(0, 999999999)
      } yield LocalTime.of(h, m, s, n)
    }

  implicit val arbLocalDateTime: Arbitrary[LocalDateTime] =
    Arbitrary {
      for {
        d <- arbitrary[LocalDate]
        t <- arbitrary[LocalTime]
      } yield LocalDateTime.of(d, t)
    }

  implicit val arbZonedDateTime: Arbitrary[ZonedDateTime] =
    Arbitrary {
      for {
        ldt <- arbitrary[LocalDateTime]
        zid <- arbitrary[ZoneId]
      } yield ZonedDateTime.of(ldt, zid)
    }

  implicit val arbInstant: Arbitrary[Instant] =
    Arbitrary(arbitrary[ZonedDateTime].map(_.toInstant))

  implicit val arbInstantMicros: Arbitrary[InstantMicros] =
    Arbitrary(arbitrary[Instant].map(InstantMicros.truncate))

  implicit val arbDuration: Arbitrary[Duration] =
    Arbitrary {
      for {
        a <- Gen.choose(-10000L, 10000L)
        u <- Gen.oneOf(ChronoUnit.values.filterNot(_.isDurationEstimated))
      } yield Duration.of(a, u)
    }

  implicit val cogInstant: Cogen[Instant] =
    Cogen[(Long, Int)].contramap(t => (t.getEpochSecond, t.getNano))

  implicit val cogInstantMicros: Cogen[InstantMicros] =
    Cogen[Instant].contramap(_.toInstant)

  implicit val cogYear: Cogen[Year] =
    Cogen[Int].contramap(_.getValue)

}
object ArbTime extends ArbTime
