// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.Epoch
import java.time.LocalDateTime
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

trait ArbEpoch {
  import ArbTime._

  implicit val arbScheme: Arbitrary[Epoch.Scheme] =
    Arbitrary(oneOf(Epoch.Julian, Epoch.Besselian))

  implicit val arbEpoch: Arbitrary[Epoch] =
    Arbitrary {
      for {
        sch <- arbitrary[Epoch.Scheme]
        ldt <- arbitrary[LocalDateTime]
      } yield sch.fromLocalDateTime(ldt)
    }

  implicit val cogEpoch: Cogen[Epoch] =
    Cogen[String].contramap(Epoch.fromString.reverseGet)

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],             // swap for a random string
      s => Gen.const(s.replace("2", "0")) // create a leading zero, maybe (ok)
    )

  // Strings that are often parsable as DMS.
  val strings: Gen[String] =
    arbitrary[Epoch].map(Epoch.fromString.reverseGet).flatMapOneOf(Gen.const, perturbations: _*)

}

object ArbEpoch extends ArbEpoch
