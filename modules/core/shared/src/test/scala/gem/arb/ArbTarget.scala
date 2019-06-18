// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gsp.math.ProperMotion
import gsp.math.arb.ArbProperMotion

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbTarget {

  import ArbEphemerisKey._
  import ArbProperMotion._

  implicit val arbTarget: Arbitrary[Target] =
    Arbitrary {
      for {
        n <- Gen.alphaStr.map(_.take(64))
        t <- arbitrary[Either[EphemerisKey, ProperMotion]]
      } yield Target(n, t)
    }

  implicit val cogTarget: Cogen[Target] =
    Cogen[(String, Either[EphemerisKey, ProperMotion])].contramap { t =>
      (t.name, t.track)
    }
}

object ArbTarget extends ArbTarget
