// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.{ RightAscension, Declination, Coordinates }
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbCoordinates {
  import ArbRightAscension._
  import ArbDeclination._

  implicit val ArbCoordinates: Arbitrary[Coordinates] =
    Arbitrary {
      for {
        ra  <- arbitrary[RightAscension]
        dec <- arbitrary[Declination]
      } yield Coordinates(ra, dec)
    }

  implicit val cogCoordinates: Cogen[Coordinates] =
    Cogen[(RightAscension, Declination)].contramap(cs => (cs.ra, cs.dec))

}
object ArbCoordinates extends ArbCoordinates
