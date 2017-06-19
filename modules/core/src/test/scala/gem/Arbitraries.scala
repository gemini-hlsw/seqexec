package gem

import gem.config.{DynamicConfig, GcalConfig, StaticConfig, TelescopeConfig}
import edu.gemini.spModel.core.{OffsetP, OffsetQ}
import gem.enum.{Instrument, SmartGcalType}
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import scalaz._
import Scalaz._

trait Arbitraries extends gem.config.Arbitraries {

  implicit val arbLocationMiddle: Arbitrary[Location.Middle] =
    Arbitrary {
      for {
        i  <- choose(Int.MinValue + 1, Int.MaxValue)
        is <- arbitrary[List[Int]]
      } yield Location.unsafeMiddle(i +: is)
    }

  implicit val arbLocation: Arbitrary[Location] =
    Arbitrary {
      Gen.frequency[Location](
        (1, Location.Beginning),
        (8, arbitrary[Location.Middle]),
        (1, Location.End)
      )
    }


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
      genBiasStepOf(i)     .widen[Step[DynamicConfig]],
      genDarkStepOf(i)     .widen[Step[DynamicConfig]],
      genGcalStepOf(i)     .widen[Step[DynamicConfig]],
      genScienceStepOf(i)  .widen[Step[DynamicConfig]],
      genSmartGcalStepOf(i).widen[Step[DynamicConfig]]
    )

  def genSequenceOf(i: Instrument): Gen[List[Step[DynamicConfig]]] =
    for {
      n <- Gen.choose(0, 50)
      s <- Gen.listOfN(n, genStepOf(i))
    } yield s


  // Observation

  def genObservationOf(i: Instrument, id: Observation.Id): Gen[Observation[StaticConfig, Step[DynamicConfig]]] =
    for {
      t <- arbitrary[String].map(_.take(255)) // probably there should be no limit?
      s <- genStaticConfigOf(i)
      d <- genSequenceOf(i)
    } yield Observation(id, t, s, d)

  def genObservation(id: Observation.Id): Gen[Observation[StaticConfig, Step[DynamicConfig]]] =
    for {
      i <- Gen.const(Instrument.Flamingos2) // Add more as they become available
      o <- genObservationOf(i, id)
    } yield o

  def genObsList(pid: Program.Id, limit: Int): Gen[List[Observation[StaticConfig, Step[DynamicConfig]]]] =
    for {
      count   <- Gen.choose(0, limit)
      obsIds  <- Gen.listOfN(count, Gen.posNum[Int]).map(_.distinct.map(i => Observation.Id(pid, i)))
      obsList <- obsIds.traverseU(genObservation)
    } yield obsList
}
