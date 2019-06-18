// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.Instrument
import gsp.math.Index
import gsp.math.arb.ArbIndex
import gsp.math.syntax.prism._
import gsp.math.syntax.treemap._

import cats.implicits._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

import scala.collection.immutable.TreeMap

trait ArbObservation {

  import ArbEnumerated._
  import ArbIndex._
  import ArbProgramId._
  import ArbStaticConfig._
  import ArbStep._
  import ArbTargetEnvironment._

  implicit val arbObservationId: Arbitrary[Observation.Id] =
    Arbitrary {
      for {
        pid <- arbitrary[ProgramId]
        num <- choose[Short](1, 100)
      } yield Observation.Id(pid, Index.fromShort.unsafeGet(num))
    }

  implicit val cogObservationId: Cogen[Observation.Id] =
    Cogen[(ProgramId, Index)].contramap(oid => (oid.pid, oid.index))

  // Generator of valid observation titles.  The schema doesn't support titles
  // longer than 255 characters and postgres doesn't want to see char 0.
  private val genTitle: Gen[String] =
    arbitrary[String].map(_.take(255).filter(_ != 0))

  def genObservationOf(i: Instrument): Gen[Observation] =
    for {
      t <- genTitle
      e <- genTargetEnvironment(i)
      s <- genStaticConfigOf(i)
      d <- genSequenceOf(i)
    } yield Observation.unsafeAssemble(t, e, s, d)

  implicit val arbObservation: Arbitrary[Observation] =
    Arbitrary {
      for {
        // We don't even try to write or read the sequence for other instruments
        // yet so including them will break ObservationDao roundtrip tests.
        i <- Gen.oneOf(Instrument.Flamingos2, Instrument.GmosN, Instrument.GmosS, Instrument.Gnirs)
        o <- genObservationOf(i)
      } yield o
    }

  def genObservationMap(limit: Int): Gen[TreeMap[Index, Observation]] =
    for {
      count   <- Gen.choose(0, limit)
      obsIdxs <- Gen.listOfN(count, Gen.posNum[Short]).map(_.distinct.map(Index.fromShort.unsafeGet))
      obsList <- obsIdxs.traverse(_ => arbitrary[Observation])
    } yield TreeMap.fromList(obsIdxs.zip(obsList))

}

object ArbObservation extends ArbObservation
