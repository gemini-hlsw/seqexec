// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import java.time.Year
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbYear {

  implicit val arbYear: Arbitrary[Year] =
    Arbitrary {
      arbitrary[Int].map { n =>
        Year.of(2010 + (n % 10).abs) // 2001-2019
      }
    }

}
object ArbYear extends ArbYear
