// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.Half
import java.time.Year
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbSemester {
  import ArbYear._
  import ArbEnumerated._

  implicit val arbSemester: Arbitrary[Semester] =
    Arbitrary {
      for {
        year <- arbitrary[Year]
        half <- arbitrary[Half]
      } yield Semester(year, half)
    }

}
object ArbSemester extends ArbSemester
