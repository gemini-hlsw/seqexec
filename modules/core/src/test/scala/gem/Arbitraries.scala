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

  private def genDynConfigOf(i: Instrument): Gen[DynamicConfig] =
    arbDynamicConfigOf(i).arbitrary

  def arbBiasStepOf(i: Instrument): Arbitrary[BiasStep[DynamicConfig]] =
    Arbitrary(genDynConfigOf(i).map(BiasStep(_)))

  def arbDarkStepOf(i: Instrument): Arbitrary[DarkStep[DynamicConfig]] =
    Arbitrary(genDynConfigOf(i).map(DarkStep(_)))

  def arbGcalStepOf(i: Instrument): Arbitrary[GcalStep[DynamicConfig]] =
    Arbitrary {
      for {
        d <- genDynConfigOf(i)
        g <- arbitrary[GcalConfig]
      } yield GcalStep(d, g)
    }

  def arbScienceStepOf(i: Instrument): Arbitrary[ScienceStep[DynamicConfig]] =
    Arbitrary(genDynConfigOf(i).map(ScienceStep(_, TelescopeConfig(OffsetP.Zero, OffsetQ.Zero))))

  def arbSmartGcalStepOf(i: Instrument): Arbitrary[SmartGcalStep[DynamicConfig]] =
    Arbitrary {
      for {
        d <- genDynConfigOf(i)
        s <- arbitrary[SmartGcalType]
      } yield SmartGcalStep(d, s)
    }

  def arbStepOf(i: Instrument): Arbitrary[Step[DynamicConfig]] =
    Arbitrary(
      Gen.oneOf(
        arbBiasStepOf(i)     .arbitrary.widen[Step[DynamicConfig]],
        arbDarkStepOf(i)     .arbitrary.widen[Step[DynamicConfig]],
        arbGcalStepOf(i)     .arbitrary.widen[Step[DynamicConfig]],
        arbScienceStepOf(i)  .arbitrary.widen[Step[DynamicConfig]],
        arbSmartGcalStepOf(i).arbitrary.widen[Step[DynamicConfig]]
      )
    )

  def arbSequenceOf(i: Instrument): Arbitrary[List[Step[DynamicConfig]]] =
    Arbitrary {
      for {
        n <- Gen.choose(0, 50)
        s <- Gen.listOfN(n, arbStepOf(i).arbitrary)
      } yield s
    }


  // Observation

  def arbObservationOf(i: Instrument, id: Observation.Id): Arbitrary[Observation[StaticConfig, Step[DynamicConfig]]] =
    Arbitrary {
      for {
        t <- arbitrary[String].map(_.take(255)) // probably there should be no limit?
        s <- arbStaticConfigOf(i).arbitrary
        d <- arbSequenceOf(i).arbitrary
      } yield Observation(id, t, s, d)
    }

  def arbObservation(id: Observation.Id): Arbitrary[Observation[StaticConfig, Step[DynamicConfig]]] =
    Arbitrary {
      for {
        i <- Gen.const(Instrument.Flamingos2) // Add more as they become available
        o <- arbObservationOf(i, id).arbitrary
      } yield o
    }
}
