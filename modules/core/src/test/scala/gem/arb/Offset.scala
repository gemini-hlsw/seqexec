// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.{ Angle, Offset }
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbOffset {
  implicit val arbOffsetP: Arbitrary[Offset.P] =
    Arbitrary(
      Gen.chooseNum(0, 10000).map(mas => Offset.P(Angle.fromMilliarcseconds(mas)))
    )

  implicit val arbOffsetQ: Arbitrary[Offset.Q] =
    Arbitrary(
      Gen.chooseNum(0, 10000).map(mas => Offset.Q(Angle.fromMilliarcseconds(mas)))
    )

  implicit val arbOffset: Arbitrary[Offset] =
    Arbitrary {
      for {
        p <- arbitrary[Offset.P]
        q <- arbitrary[Offset.Q]
      } yield Offset(p, q)
    }

}
object ArbOffset extends ArbOffset
