// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.{ chooseNum, const, oneOf }
import org.scalacheck.Cogen._

trait ArbProperMotion {
  import ArbEpoch._
  import ArbCoordinates._
  import ArbOffset._
  import ArbRadialVelocity._
  import ArbAngle._

  implicit val arbProperMotion: Arbitrary[ProperMotion] =
    Arbitrary {
      for {
        cs <- arbitrary[Coordinates]
        ap <- arbitrary[Epoch]
        pv <- arbitrary[Option[Offset]]
        rv <- arbitrary[Option[RadialVelocity]]
        px <- oneOf(
                const(Option.empty[Angle]),
                // Limit to a value that will fit in numeric(9,3) milliseconds.
                chooseNum(-999999000L, 999999000L).map(µas => Some(Angle.fromMicroarcseconds(µas)))
              )
      } yield ProperMotion(cs, ap, pv, rv, px)
    }

  implicit val cogProperMotion: Cogen[ProperMotion] =
    Cogen[(Coordinates, Epoch, Option[Offset], Option[RadialVelocity], Option[Angle])].contramap { p =>
      (p.baseCoordinates, p.epoch, p.properVelocity, p.radialVelocity, p.parallax)
    }

}

object ArbProperMotion extends ArbProperMotion
