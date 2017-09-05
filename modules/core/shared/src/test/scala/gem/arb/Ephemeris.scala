// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.{ Coordinates, Ephemeris }
import gem.util.InstantMicros

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

trait ArbEphemeris {
  import ArbCoordinates._
  import ArbTime._
  import Ephemeris.Element

  implicit val arbElement: Arbitrary[Element] =
    Arbitrary {
      for {
        t  <- arbitrary[InstantMicros]
        cs <- arbitrary[Coordinates]
      } yield (t, cs)
    }

  implicit val arbEphemeris: Arbitrary[Ephemeris] =
    Arbitrary {
      for {
        len <- choose(0, 100)
        es  <- listOfN(len, arbitrary[Element])
      } yield Ephemeris(es: _*)
    }

  implicit val cogEphemeris: Cogen[Ephemeris] =
    Cogen[Map[InstantMicros, Coordinates]].contramap(_.toMap)

}
object ArbEphemeris extends ArbEphemeris
