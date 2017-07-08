// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum. { Site, ProgramType }
import java.time.LocalDate
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbProgramId {
  import ProgramId._
  import ArbEnumerated._
  import ArbSemester._
  import ArbLocalDate._

  implicit val arbScience: Arbitrary[Science] =
    Arbitrary {
      for {
        site        <- arbitrary[Site]
        semester    <- arbitrary[Semester]
        programType <- arbitrary[ProgramType]
        index       <- arbitrary[Int].map(n => (n % 200).abs + 1)
      } yield Science.unsafeApply(site, semester, programType, index)
    }

  implicit val arbDaily: Arbitrary[Daily] =
    Arbitrary {
      for {
        site        <- arbitrary[Site]
        programType <- arbitrary[ProgramType]
        localDate   <- arbitrary[LocalDate]
      } yield Daily(site, programType, localDate)
    }

}
object ArbProgramId extends ArbProgramId
