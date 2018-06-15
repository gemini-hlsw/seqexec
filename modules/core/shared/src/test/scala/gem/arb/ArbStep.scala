// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.SmartGcalType
import gem.Step
import gem.config.{ DynamicConfig, GcalConfig, TelescopeConfig }
import gem.math.Offset
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbStep extends Arbitraries {
  import ArbEnumerated._
  import ArbOffset._

  val genTelescopeConfig: Gen[TelescopeConfig] =
    for {
      p <- arbitrary[Offset.P]
      q <- arbitrary[Offset.Q]
    } yield TelescopeConfig(p, q)

  val genBase: Gen[Step.Base] =
    Gen.oneOf(
      Gen.const(Step.Base.Bias),
      Gen.const(Step.Base.Dark),
      arbitrary[GcalConfig].map(Step.Base.Gcal(_)),
      genTelescopeConfig.map(Step.Base.Science(_)),
      arbitrary[SmartGcalType].map(Step.Base.SmartGcal)
    )

  implicit val arbStep: Arbitrary[Step] =
    Arbitrary {
      for {
        dc <- arbitrary[DynamicConfig]
        b  <- genBase
      } yield dc.toStep(b)
    }

}
object ArbStep extends ArbStep
