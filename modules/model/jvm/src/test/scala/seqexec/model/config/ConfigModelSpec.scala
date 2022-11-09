// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import lucuma.core.util.arb.ArbEnumerated._
import seqexec.model.config.arb.ArbSystemsControlConfiguration._

/**
 * Tests config classes
 */
final class ConfigModelSpec extends CatsSuite {
  checkAll("Eq[SystemsControlConfiguration]", EqTests[SystemsControlConfiguration].eqv)
  checkAll("Eq[Mode]", EqTests[Mode].eqv)
  // checkAll("Eq[AuthenticationConfig]", EqTests[AuthenticationConfig].eqv)
}
