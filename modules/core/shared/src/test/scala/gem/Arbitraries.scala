// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.implicits._
import gem.arb._
import gem.config.{ DynamicConfig, GcalConfig, TelescopeConfig }
import gem.enum.{Instrument, SmartGcalType}
import gem.math.Offset
import gem.syntax.treemap._
import gem.util.Location
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import scala.collection.immutable.TreeMap

trait Arbitraries extends gem.config.Arbitraries  {
  import ArbEnumerated._
  import ArbTargetEnvironment._

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

  implicit val cogLocation: Cogen[Location] =
    Cogen[String].contramap(_.toString)

  // Generator of valid observation/program titles.  The schema doesn't support
  // titles longer than 255 characters and postgres doesn't want to see char 0.
  val genTitle: Gen[String] =
    arbitrary[String].map(_.take(255).filter(_ != 0))


  // Step and Sequence

  def genBiasStepOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[Step.Bias[DynamicConfig.Aux[I]]] =
    genDynamicConfigOf(i).map(Step.Bias(_))

  def genDarkStepOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[Step.Dark[DynamicConfig.Aux[I]]] =
    genDynamicConfigOf(i).map(Step.Dark(_))

  def genGcalStepOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[Step.Gcal[DynamicConfig.Aux[I]]] =
    for {
      d <- genDynamicConfigOf(i)
      g <- arbitrary[GcalConfig]
    } yield Step.Gcal(d, g)

  def genScienceStepOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[Step.Science[DynamicConfig.Aux[I]]] =
    genDynamicConfigOf(i).map(Step.Science(_, TelescopeConfig(Offset.P.Zero, Offset.Q.Zero)))

  def genSmartGcalStepOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[Step.SmartGcal[DynamicConfig.Aux[I]]] =
    for {
      d <- genDynamicConfigOf(i)
      s <- arbitrary[SmartGcalType]
    } yield Step.SmartGcal(d, s)

  def genStepOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[Step[DynamicConfig.Aux[I]]] =
    Gen.oneOf(
      genBiasStepOf(i),
      genDarkStepOf(i),
      genGcalStepOf(i),
      genScienceStepOf(i),
      genSmartGcalStepOf(i)
    )

  def genSequenceOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[List[Step[DynamicConfig.Aux[I]]]] =
    for {
      n <- Gen.choose(0, 50)
      s <- Gen.listOfN(n, genStepOf(i))
    } yield s


  // Observation

  def genObservationOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[Observation.Full] =
    for {
      t <- genTitle
      e <- genTargetEnvironment(i)
      s <- genStaticConfigOf(i)
      d <- genSequenceOf(i)
    } yield Observation(t, e, s, d)

  implicit val arbObservation: Arbitrary[Observation.Full] =
    Arbitrary {
      for {
        i <- Gen.oneOf(
               Instrument.Flamingos2,
               Instrument.GmosN,
               Instrument.GmosS
             ) // Add more as they become available
        o <- genObservationOf(i: Instrument.Aux[i.type])
      } yield o
    }

  def genObservationMap(limit: Int): Gen[TreeMap[Observation.Index, Observation.Full]] =
    for {
      count   <- Gen.choose(0, limit)
      obsIdxs <- Gen.listOfN(count, Gen.posNum[Short]).map(_.distinct.map(Observation.Index.unsafeFromShort))
      obsList <- obsIdxs.traverse(_ => arbitrary[Observation.Full])
    } yield TreeMap.fromList(obsIdxs.zip(obsList))
}
