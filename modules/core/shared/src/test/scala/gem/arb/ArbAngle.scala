// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.{ Angle, HourAngle }
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbAngle {

  implicit def arbAngle: Arbitrary[Angle] =
    Arbitrary(arbitrary[Double].map(Angle.fromDoubleDegrees))

  implicit def arbHourAngle: Arbitrary[HourAngle] =
    Arbitrary(arbitrary[Double].map(HourAngle.fromDoubleHours))

  implicit def cogAngle: Cogen[Angle] =
    Cogen[Double].contramap(_.toDoubleDegrees)

  implicit def cogHourAngle: Cogen[HourAngle] =
    Cogen[Double].contramap(_.toDoubleDegrees)

  private val perturbations: List[String => Gen[String]] =
    List(
      s => arbitrary[String],             // swap for a random string
      s => Gen.const(s.replace(":", " ")) // replace colons with spaces (ok)
    )

  // Strings that are often parsable as HMS.
  val stringsHMS: Gen[String] =
    arbitrary[HourAngle].map(_.formatHMS).flatMapOneOf(Gen.const, perturbations: _*)

  // Strings that are often parsable as DMS.
  val stringsDMS: Gen[String] =
    arbitrary[Angle].map(_.formatDMS).flatMapOneOf(Gen.const, perturbations: _*)

}

object ArbAngle extends ArbAngle
