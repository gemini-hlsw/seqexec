// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.util.Timestamp

import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbEphemerisMeta {

  import ArbTimestamp._

  implicit val arbHorizonsSolnRef: Arbitrary[HorizonsSolutionRef] =
    Arbitrary {
      Gen.alphaStr.map(HorizonsSolutionRef.apply)
    }

  implicit val arbEphemerisMeta: Arbitrary[EphemerisMeta] =
    Arbitrary {
      for {
        u <- arbitrary[Timestamp]
        c <- arbitrary[Timestamp]
        s <- arbitrary[Option[HorizonsSolutionRef]]
      } yield EphemerisMeta(u, c, s)
    }

}

object ArbEphemerisMeta extends ArbEphemerisMeta
