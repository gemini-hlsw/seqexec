// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.implicits._
import gem.arb._
import gem.config.{ GcalConfig, TelescopeConfig }
import gem.enum.{Instrument, SmartGcalType}
import gem.math.{ Index, Offset }
import gem.syntax.all._
import org.scalacheck._
import org.scalacheck.Arbitrary._

import scala.collection.immutable.TreeMap

trait Arbitraries extends gem.config.Arbitraries {
  import ArbGcalConfig._
  import ArbEnumerated._
  import ArbTargetEnvironment._

  // Generator of valid observation/program titles.  The schema doesn't support
  // titles longer than 255 characters and postgres doesn't want to see char 0.
  val genTitle: Gen[String] =
    arbitrary[String].map(_.take(255).filter(_ != 0))

  // Step and Sequence

  def genBiasStepOf(i: Instrument): Gen[Step] =
    genDynamicConfigOf(i).map(_.toStep(Step.Base.Bias))

  def genDarkStepOf(i: Instrument): Gen[Step] =
    genDynamicConfigOf(i).map(_.toStep(Step.Base.Dark))

  def genGcalStepOf(i: Instrument): Gen[Step] =
    for {
      d <- genDynamicConfigOf(i)
      g <- arbitrary[GcalConfig]
    } yield d.toStep(Step.Base.Gcal(g))

  def genScienceStepOf(i: Instrument): Gen[Step] =
    genDynamicConfigOf(i).map(_.toStep(Step.Base.Science(TelescopeConfig(Offset.P.Zero, Offset.Q.Zero))))

  def genSmartGcalStepOf(i: Instrument): Gen[Step] =
    for {
      d <- genDynamicConfigOf(i)
      s <- arbitrary[SmartGcalType]
    } yield d.toStep(Step.Base.SmartGcal(s))

  def genStepOf(i: Instrument): Gen[Step] =
    Gen.oneOf(
      genBiasStepOf(i),
      genDarkStepOf(i),
      genGcalStepOf(i),
      genScienceStepOf(i),
      genSmartGcalStepOf(i)
    )

  def genSequenceOf(i: Instrument): Gen[List[Step]] =
    for {
      n <- Gen.choose(0, 50)
      s <- Gen.listOfN(n, genStepOf(i))
    } yield s

  // Observation

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
        i <- Gen.oneOf(
               Instrument.Flamingos2,
               Instrument.GmosN,
               Instrument.GmosS,
             ) // Add more as they become available
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
