// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.util

import gem.arb._

import cats.kernel.laws.discipline._
import cats.laws.discipline._
import cats.tests.CatsSuite


final class ZipperSpec extends CatsSuite {

  import ArbZipper._

  checkAll("Eq",          EqTests[Zipper[Byte]].eqv)
  checkAll("Applicative", ApplicativeTests[Zipper].applicative[Byte, Byte, Byte])

}
