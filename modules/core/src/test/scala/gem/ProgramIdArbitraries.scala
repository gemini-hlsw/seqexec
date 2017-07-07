// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum._
import java.time. { Year, LocalDate }
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ProgramIdArbitraries extends gem.enum.Arbitraries {
  import ProgramId._

  implicit val arbYear: Arbitrary[Year] =
    Arbitrary {
      arbitrary[Int].map { n =>
        Year.of(2010 + (n % 10).abs) // 2001-2019
      }
    }

  implicit val arbLocalDate: Arbitrary[LocalDate] =
    Arbitrary {
      for {
        y <- arbitrary[Year]
        d <- arbitrary[Int].map(n => (n % 364).abs + 1)
      } yield LocalDate.ofYearDay(y.getValue, d)
    }

  implicit val arbSemester: Arbitrary[Semester] =
    Arbitrary {
      for {
        year <- arbitrary[Year]
        half <- arbitrary[Half]
      } yield Semester(year, half)
    }

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
