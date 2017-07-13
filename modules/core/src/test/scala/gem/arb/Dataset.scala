// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import java.time.Instant
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

trait ArbDataset {
  import ArbObservation._
  import ArbTime._

  implicit val arbLabel: Arbitrary[Dataset.Label] =
    Arbitrary {
      for {
        oid <- arbitrary[Observation.Id]
        idx <- choose(1,1000)
      } yield Dataset.Label(oid, idx)
    }

  implicit val adbDataset: Arbitrary[Dataset] =
    Arbitrary {
      for {
        lab <- arbitrary[Dataset.Label]
        fn  <- arbitrary[String].map(_.take(100).filterNot(_ == 0))
        ts  <- arbitrary[Instant]
      } yield Dataset(lab, fn, ts)
    }

}
object ArbDataset extends ArbDataset
