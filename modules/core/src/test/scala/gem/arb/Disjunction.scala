// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scalaz._
import Scalaz._

trait ArbDisjunction {

  implicit def arbDisjunction[A: Arbitrary, B: Arbitrary]: Arbitrary[A \/ B] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[A].map(_.left[B] ),
        arbitrary[B].map(_.right[A])
      )
    }
}
object ArbDisjunction extends ArbDisjunction
