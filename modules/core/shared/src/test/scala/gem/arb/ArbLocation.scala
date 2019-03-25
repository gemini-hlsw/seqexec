// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.arb

import gem.util.Location

import cats.implicits._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen}

trait ArbLocation {

  implicit val arbLocationMiddle: Arbitrary[Location.Middle] =
    Arbitrary {
      for {
        i  <- Gen.choose(Int.MinValue + 1, Int.MaxValue)
        is <- arbitrary[List[Int]]
      } yield Location.unsafeMiddleFromFoldable(i +: is)
    }

  implicit val arbLocation: Arbitrary[Location] =
    Arbitrary {
      Gen.frequency[Location](
        (1, Location.Beginning),
        (8, arbitrary[Location.Middle]),
        (1, Location.End)
      )
    }

  implicit val cogLocation: Cogen[Location] =
    Cogen[String].contramap(_.toString)

}

object ArbLocation extends ArbLocation
