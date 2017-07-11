// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.config.{DynamicConfig, GcalConfig, StaticConfig, TelescopeConfig}
import edu.gemini.spModel.core.{OffsetP, OffsetQ}
import gem.enum.{Instrument, SmartGcalType}
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import scalaz._
import Scalaz._

trait Arbitraries extends gem.config.Arbitraries  {
  import gem.arb.ArbEnumerated._

  implicit val arbLocationMiddle: Arbitrary[Location.Middle] =
    Arbitrary {
      for {
        i  <- choose(Int.MinValue + 1, Int.MaxValue)
        is <- arbitrary[List[Int]]
      } yield Location.unsafeMiddleFromFoldable(i +: is)
    }

  implicit val arbLocation: Arbitrary[Location] =
    Arbitrary {
      Gen.frequency[Location](
        (1, Location.Beginning),
        (8, arbitrary[Location.Middle]),
        (1, Location.End)
      )
    }

  // Generator of valid observation/program titles.  The schema doesn't support
  // titles longer than 255 characters and postgres doesn't want to see char 0.
  val genTitle: Gen[String] =
    arbitrary[String].map(_.take(255).filter(_ != 0))


  // Step and Sequence

  def genBiasStepOf(i: Instrument): Gen[BiasStep[DynamicConfig]] =
    genDynamicConfigOf(i).map(BiasStep(_))

  def genDarkStepOf(i: Instrument): Gen[DarkStep[DynamicConfig]] =
    genDynamicConfigOf(i).map(DarkStep(_))

  def genGcalStepOf(i: Instrument): Gen[GcalStep[DynamicConfig]] =
    for {
      d <- genDynamicConfigOf(i)
      g <- arbitrary[GcalConfig]
    } yield GcalStep(d, g)

  def genScienceStepOf(i: Instrument): Gen[ScienceStep[DynamicConfig]] =
    genDynamicConfigOf(i).map(ScienceStep(_, TelescopeConfig(OffsetP.Zero, OffsetQ.Zero)))

  def genSmartGcalStepOf(i: Instrument): Gen[SmartGcalStep[DynamicConfig]] =
    for {
      d <- genDynamicConfigOf(i)
      s <- arbitrary[SmartGcalType]
    } yield SmartGcalStep(d, s)

  def genStepOf(i: Instrument): Gen[Step[DynamicConfig]] =
    Gen.oneOf(
      genBiasStepOf(i),
      genDarkStepOf(i),
      genGcalStepOf(i),
      genScienceStepOf(i),
      genSmartGcalStepOf(i)
    )

  def genSequenceOf(i: Instrument): Gen[List[Step[DynamicConfig]]] =
    for {
      n <- Gen.choose(0, 50)
      s <- Gen.listOfN(n, genStepOf(i))
    } yield s


  // Observation

  def genObservationOf(i: Instrument, id: Observation.Id): Gen[Observation[StaticConfig, Step[DynamicConfig]]] =
    for {
      t <- genTitle
      s <- genStaticConfigOf(i)
      d <- genSequenceOf(i)
    } yield Observation(id, t, s, d)

  def genObservation(id: Observation.Id): Gen[Observation[StaticConfig, Step[DynamicConfig]]] =
    for {
      i <- Gen.oneOf(
             Instrument.Flamingos2,
             Instrument.GmosN,
             Instrument.GmosS
           ) // Add more as they become available
      o <- genObservationOf(i, id)
    } yield o

  def genObservationList(pid: Program.Id, limit: Int): Gen[List[Observation[StaticConfig, Step[DynamicConfig]]]] =
    for {
      count   <- Gen.choose(0, limit)
      obsIds  <- Gen.listOfN(count, Gen.posNum[Int]).map(_.distinct.map(i => Observation.Id(pid, i)))
      obsList <- obsIds.traverseU(genObservation)
    } yield obsList
}
