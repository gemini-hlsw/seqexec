// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.MagnitudeValue
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbMagnitudeValue {

  implicit val arbMagnitudeValue: Arbitrary[MagnitudeValue] = Arbitrary {
    arbitrary[Int].map(MagnitudeValue.apply)
  }

  implicit val cogMagnitudeValue: Cogen[MagnitudeValue] =
    Cogen[Int].contramap(_.value)

}

object ArbMagnitudeValue extends ArbMagnitudeValue
