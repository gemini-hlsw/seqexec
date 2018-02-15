// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import Model._
import cats.tests.CatsSuite
import cats.kernel.laws.discipline._

/**
  * Tests Model typeclasses
  */
final class ModelSpec extends CatsSuite {
  import SharedModelArbitraries._

  checkAll("Eq[SystemName]", EqTests[SystemName].eqv)
}
