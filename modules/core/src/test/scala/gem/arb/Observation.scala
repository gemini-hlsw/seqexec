// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

trait ArbObservation {
  import ArbProgramId._

  implicit val arbObservationId: Arbitrary[Observation.Id] =
    Arbitrary {
      for {
        pid <- arbitrary[ProgramId]
        num <- choose(1, 100)
      } yield Observation.Id(pid, num)
    }

}
object ArbObservation extends ArbObservation
