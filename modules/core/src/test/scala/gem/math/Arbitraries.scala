// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import org.scalacheck._
import org.scalacheck.Arbitrary._

trait Arbitraries {

  implicit def arbAngle: Arbitrary[Angle] =
    Arbitrary(arbitrary[Double].map(Angle.fromDoubleDegrees))

  implicit def arbHourAngle: Arbitrary[HourAngle] =
    Arbitrary(arbitrary[Double].map(HourAngle.fromDoubleHours))

}
