// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import gem.arb._
import monocle.law.discipline._

final class IndexSpec extends CatsSuite {
  import ArbIndex._

  // Laws
  checkAll("Index", OrderTests[Index].order)
  checkAll("Index.fromShort", PrismTests(Index.fromShort))
  checkAll("Index.fromString", PrismTests(Index.fromString))

}
