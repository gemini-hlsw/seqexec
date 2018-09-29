// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
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

  implicit val arbDataset: Arbitrary[Dataset] =
    Arbitrary {
      for {
        lab <- arbitrary[Dataset.Label]
        fn  <- arbitrary[String].map(_.take(100).filterNot(_ == 0))
        ts  <- arbitrary[Instant]
      } yield Dataset(lab, fn, ts)
    }

  implicit val cogLabel: Cogen[Dataset.Label] =
    Cogen[(Observation.Id, Int)].contramap(l => (l.observationId, l.index))

  implicit val cogDataset: Cogen[Dataset] =
    Cogen[(Dataset.Label, String, Instant)].contramap(ds => (ds.label, ds.filename, ds.timestamp))

  // Strings that are often parsable as Labels
  val strings: Gen[String] =
    arbitrary[Dataset.Label].map(Dataset.Label.fromString.reverseGet).flatMapOneOf(
      Gen.const,                            // Do nothing, or
      _ => arbitrary[String],               // Replace with an arbitrary String, or
      s => Gen.const(s.replace("-0", "-")), // remove a leading zero.
    )

}

object ArbDataset extends ArbDataset
