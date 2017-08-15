// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.{ Angle, Offset }
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbOffset {
  import ArbAngle._

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

  implicit val cogOffsetP: Cogen[Offset.P] =
    Cogen[Angle].contramap(_.toAngle)

  implicit val cogOffsetQ: Cogen[Offset.Q] =
    Cogen[Angle].contramap(_.toAngle)

  implicit val cogOffset: Cogen[Offset] =
    Cogen[(Offset.P, Offset.Q)].contramap(o => (o.p, o.q))

}
object ArbOffset extends ArbOffset
