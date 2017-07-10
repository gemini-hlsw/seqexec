// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import java.time._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import scala.collection.JavaConverters._

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
        d <- choose(1, 365)
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

}
object ArbTime extends ArbTime
