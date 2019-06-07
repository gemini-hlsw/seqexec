// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.config.TelescopeConfig
import gsp.math.Offset
import gsp.math.arb.ArbOffset

import org.scalacheck.{ Arbitrary, Cogen }
import org.scalacheck.Arbitrary.arbitrary

trait ArbTelescopeConfig {

  import ArbOffset._

  implicit val arbTelescopeConfig: Arbitrary[TelescopeConfig] =
    Arbitrary {
      arbitrary[Offset].map(o => TelescopeConfig(o.p, o.q))
    }

  implicit val cogTelescopeConfig: Cogen[TelescopeConfig] =
    Cogen[(Offset.P, Offset.Q)].contramap(t => (t.p, t.q))

}

object ArbTelescopeConfig extends ArbTelescopeConfig
