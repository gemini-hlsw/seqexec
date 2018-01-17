// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbTargetEnvironment {

  import ArbUserTarget._

  implicit val arbTargetEnvironment: Arbitrary[TargetEnvironment] =
    Arbitrary {
      for {
        len <- choose(0, 10)
        uts <- listOfN(len, arbitrary[UserTarget])
      } yield TargetEnvironment(uts.toSet)
    }

  implicit val cogTargetEnvironment: Cogen[TargetEnvironment] =
    Cogen[List[UserTarget]].contramap(_.userTargets.toList)
}

object ArbTargetEnvironment extends ArbTargetEnvironment