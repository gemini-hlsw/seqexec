// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.UserTargetType
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbUserTarget {

  import ArbEnumerated._
  import ArbTarget._

  implicit val arbUserTarget: Arbitrary[UserTarget] =
    Arbitrary {
      for {
        t <- arbitrary[Target]
        y <- arbitrary[UserTargetType]
      } yield UserTarget(t, y)
    }
}

object ArbUserTarget extends ArbUserTarget
