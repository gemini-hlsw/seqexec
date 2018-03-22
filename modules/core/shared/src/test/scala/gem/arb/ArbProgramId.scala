// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum. { Site, ProgramType, DailyProgramType }
import gem.math.Index
import gem.syntax.prism._
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
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  implicit val arbNonstandard: Arbitrary[Nonstandard] =
    Arbitrary {

      val gen = for {
        site        <- arbitrary[Option[Site]]
        semester    <- arbitrary[Option[Semester]]
        programType <- arbitrary[Option[ProgramType]]
        tail        <- Gen.alphaNumStr
      } yield ProgramId.fromString(Nonstandard.format(site, semester, programType, tail))

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
    Cogen[String].contramap(_.format)

  private val perturbations: List[String => Gen[String]] =
    List(
      s => arbitrary[String],                          // swap for a random string
      s => Gen.const("\\d+$".r.replaceAllIn(s, "0$0")) // add a leading zero to the index
    )

  // Strings that are often parsable as a program id.
  val stringsScience: Gen[String] =
    arbitrary[Science].map(_.format).flatMapOneOf(Gen.const, perturbations: _*)

}

object ArbProgramId extends ArbProgramId
