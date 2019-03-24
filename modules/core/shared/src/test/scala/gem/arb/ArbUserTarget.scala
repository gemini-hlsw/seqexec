// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.UserTargetType
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

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

  implicit val cogUserTarget: Cogen[UserTarget] =
    Cogen[(Target, UserTargetType)].contramap { u =>
      (u.target, u.targetType)
    }
}

object ArbUserTarget extends ArbUserTarget
