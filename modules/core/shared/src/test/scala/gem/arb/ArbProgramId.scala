// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum. { Site, ProgramType, DailyProgramType }
import gsp.math.Index
import gsp.math.arb.ArbTime
import gsp.math.syntax.prism._
import java.time.LocalDate
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

trait ArbProgramId {
  import ProgramId._
  import ArbEnumerated._
  import ArbSemester._
  import ArbTime._

  implicit val arbScience: Arbitrary[Science] =
    Arbitrary {
      for {
        site        <- arbitrary[Site]
        semester    <- arbitrary[Semester]
        programType <- arbitrary[ProgramType]
        index       <- choose[Short](1, 200).map(Index.fromShort.unsafeGet)
      } yield Science(site, semester, programType, index)
    }

  implicit val arbDaily: Arbitrary[Daily] =
    Arbitrary {
      for {
        site        <- arbitrary[Site]
        programType <- arbitrary[DailyProgramType]
        localDate   <- arbitrary[LocalDate]
      } yield Daily(site, programType, localDate)
    }

  // This one is a little awkward because the only way we can get a Nonstandard is via parsing.
  implicit val arbNonstandard: Arbitrary[Nonstandard] =
    Arbitrary {

      val gen = for {
        site        <- arbitrary[Option[Site]]
        semester    <- arbitrary[Option[Semester]]
        programType <- arbitrary[Option[ProgramType]]
        tail        <- Gen.alphaNumStr
      } yield ProgramId.fromString.getOption(Nonstandard.format(site, semester, programType, tail))

      // It's possible that the generated value is actually a valid Science id, so we need to
      // retry until we get a Nonstandard.
      gen.collectUntil { case Some(nid: Nonstandard) => nid }

    }

  implicit val arbProgramId: Arbitrary[ProgramId] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[Science]    .map[ProgramId](identity),
        arbitrary[Daily]      .map[ProgramId](identity),
        arbitrary[Nonstandard].map[ProgramId](identity)
      )
    }

  implicit val cogProgramId: Cogen[ProgramId] =
    Cogen[String].contramap(ProgramId.fromString.reverseGet)

  implicit val cogScience: Cogen[ProgramId.Science] =
    Cogen[String].contramap(ProgramId.Science.fromString.reverseGet)

  implicit val cogDaily: Cogen[ProgramId.Daily] =
    Cogen[String].contramap(ProgramId.Daily.fromString.reverseGet)

  implicit val cogNonstandard: Cogen[ProgramId.Nonstandard] =
    Cogen[String].contramap(ProgramId.Nonstandard.fromString.reverseGet)

}

object ArbProgramId extends ArbProgramId
