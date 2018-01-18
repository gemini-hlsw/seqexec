// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbTarget {

  import ArbTrack._

  implicit val arbTarget: Arbitrary[Target] =
    Arbitrary {
      for {
        n <- Gen.alphaStr.map(_.take(64))
        t <- arbitrary[Track]
      } yield Target(n, t)
    }

  implicit val cogTarget: Cogen[Target] =
    Cogen[(String, Track)].contramap { t =>
      (t.name, t.track)
    }
}

object ArbTarget extends ArbTarget
