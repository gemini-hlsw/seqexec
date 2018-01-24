// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbObservation {
  import ArbProgramId._

  implicit val arbObservationId: Arbitrary[Observation.Id] =
    Arbitrary {
      for {
        pid <- arbitrary[ProgramId]
        num <- choose[Short](1, 100)
      } yield Observation.Id(pid, Observation.Index.unsafeFromShort(num))
    }

  implicit val cogObservationIdex: Cogen[Observation.Index] =
    Cogen[Short].contramap(_.toShort)

  implicit val cogObservationId: Cogen[Observation.Id] =
    Cogen[(ProgramId, Observation.Index)].contramap(oid => (oid.pid, oid.index))

}

object ArbObservation extends ArbObservation
