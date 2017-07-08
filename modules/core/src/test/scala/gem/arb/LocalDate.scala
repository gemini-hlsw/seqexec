// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import java.time.{ LocalDate, Year }
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbLocalDate {
  import ArbYear._

  implicit val arbLocalDate: Arbitrary[LocalDate] =
    Arbitrary {
      for {
        y <- arbitrary[Year]
        d <- arbitrary[Int].map(n => (n % 364).abs + 1)
      } yield LocalDate.ofYearDay(y.getValue, d)
    }

}
object ArbLocalDate extends ArbLocalDate
