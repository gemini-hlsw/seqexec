// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.util.Timestamp

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

  /** An arbitrary `Timestamp` from `Timestamp.Epoch` to `Timestamp.Max`. We
    * don't generate from `Timestamp.Min` due to what appears to be a
    * `java.sql.Timestamp` formatting issue for ancient dates.  See Issue #241.
    */
  implicit val arbTimestamp: Arbitrary[Timestamp] =
    Arbitrary {
      for {
        m <- Gen.choose(0L, Duration.between(Instant.EPOCH, Timestamp.Max.toInstant).toMillis)
        u <- Gen.choose(0, 999L)
      } yield Timestamp.Epoch.plusMillis(m).flatMap(_.plusMicros(u)).getOrElse(Timestamp.Epoch)
    }

  implicit val arbDuration: Arbitrary[Duration] =
    Arbitrary {
      for {
        a <- Gen.choose(-10000L, 10000L)
        u <- Gen.oneOf(ChronoUnit.values.filterNot(_.isDurationEstimated))
      } yield Duration.of(a, u)
    }

  implicit val cogInstant: Cogen[Instant] =
    Cogen[(Long, Int)].contramap(t => (t.getEpochSecond, t.getNano))

  implicit val cogLocalDate: Cogen[LocalDate] =
    Cogen[(Int, Int)].contramap(d => (d.getYear, d.getDayOfYear))

  implicit val cogTimestamp: Cogen[Timestamp] =
    Cogen[Instant].contramap(_.toInstant)

  implicit val cogYear: Cogen[Year] =
    Cogen[Int].contramap(_.getValue)

}

object ArbTime extends ArbTime
