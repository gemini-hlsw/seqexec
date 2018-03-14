// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.Instrument
import gem.math.Index
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
      } yield Observation.Id(pid, Index.unsafeFromShort(num))
    }

  def genObservation[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[Observation.Full] =
    for {
      t   <- arbitrary[String]
      te  <- genTargetEnvironment(i)
      sc  <- genStaticConfigOf(i)
      dc  <- genDynamicConfigOf(i)
    } yield Observation(t, te, sc, List(Step.Bias(dc)))

  implicit val arbObservation: Arbitrary[Observation.Full] =
    Arbitrary {
      for {
        i <- arbitrary[Instrument]
        o <- genObservation[i.type](i)
      } yield o
    }

  implicit val cogObservationId: Cogen[Observation.Id] =
    Cogen[(ProgramId, Index)].contramap(oid => (oid.pid, oid.index))

}

object ArbObservation extends ArbObservation
