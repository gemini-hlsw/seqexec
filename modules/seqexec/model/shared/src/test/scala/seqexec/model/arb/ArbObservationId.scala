// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

import seqexec.model.ProgramId
import lucuma.core.math.arb._
import seqexec.model.Observation
import seqexec.model.arb.ArbProgramId
import lucuma.core.math.Index
import lucuma.core.optics.syntax.prism._

trait ArbObservationId {

  import ArbIndex._
  import ArbProgramId._

  implicit val arbObservationId: Arbitrary[Observation.Id] =
    Arbitrary {
      for {
        pid <- arbitrary[ProgramId]
        num <- choose[Short](1, 100)
      } yield Observation.Id(pid, Index.fromShort.unsafeGet(num))
    }

  implicit val cogObservationId: Cogen[Observation.Id] =
    Cogen[(ProgramId, Index)].contramap(oid => (oid.pid, oid.index))

}

object ArbObservationId extends ArbObservationId
