// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.tests.CatsSuite

import cats.kernel.laws.discipline._
import gem.arb._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class TargetEnvironmentSpec extends CatsSuite {
  import ArbTargetEnvironment._

  // laws
  checkAll("TargetEnvironment", EqTests[TargetEnvironment].eqv)
}
