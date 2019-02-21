// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.{Instrument, SmartGcalType}
import gem.config.{GcalConfig, TelescopeConfig}
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbStep extends Arbitraries {

  import ArbEnumerated._
  import ArbGcalConfig._
  import ArbDynamicConfig._
  import ArbTelescopeConfig._

  implicit val arbStepBase: Arbitrary[Step.Base] =
    Arbitrary {
      Gen.oneOf(
        Gen.const(Step.Base.Bias),
        Gen.const(Step.Base.Dark),
        arbitrary[GcalConfig].map(Step.Base.Gcal(_)),
        arbitrary[TelescopeConfig].map(Step.Base.Science(_)),
        arbitrary[SmartGcalType].map(Step.Base.SmartGcal)
      )
    }

  def genStepOf(i: Instrument): Gen[Step] =
    for {
      b <- arbitrary[Step.Base]
      d <- genDynamicConfigOf(i)
    } yield d.toStep(b)

  implicit val arbStep: Arbitrary[Step] =
    Arbitrary(arbitrary[Instrument].flatMap(genStepOf))

  def genSequenceOf(i: Instrument): Gen[List[Step]] =
    for {
      n <- Gen.choose(0, 50)
      s <- Gen.listOfN(n, genStepOf(i))
    } yield s

}
object ArbStep extends ArbStep
