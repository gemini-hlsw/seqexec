// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.Instrument
import gem.math.Index
import gem.syntax.prism._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbObservation extends gem.config.Arbitraries {
  import ArbEnumerated._
  import ArbIndex._
  import ArbProgramId._
  import ArbTargetEnvironment._

  implicit val arbObservationId: Arbitrary[Observation.Id] =
    Arbitrary {
      for {
        pid <- arbitrary[ProgramId]
        num <- choose[Short](1, 100)
      } yield Observation.Id(pid, Index.fromShort.unsafeGet(num))
    }

  def genObservation(i: Instrument): Gen[Observation] =
    for {
      t   <- arbitrary[String]
      te  <- genTargetEnvironment(i)
      sc  <- genStaticConfigOf(i)
      dc  <- genDynamicConfigOf(i)
    } yield Observation.unsafeAssemble(t, te, sc, List(dc.toStep(Step.Base.Bias)))

  implicit val arbObservation: Arbitrary[Observation] =
    Arbitrary {
      for {
        i <- arbitrary[Instrument]
        o <- genObservation(i)
      } yield o
    }

  implicit val cogObservationId: Cogen[Observation.Id] =
    Cogen[(ProgramId, Index)].contramap(oid => (oid.pid, oid.index))

}

object ArbObservation extends ArbObservation
