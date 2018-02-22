// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package optics

import cats.tests.CatsSuite
import gem.laws.discipline._

final class SplitEpiSpec extends CatsSuite {

  val ex1: SplitEpi[Long, Int] =
    SplitEpi(_.toInt, _.toLong)

  val ex2: SplitEpi[Int, Byte] =
    SplitEpi(_.toByte, _.toInt)

  // Laws
  checkAll("Long > Int", SplitEpiTests(ex1).splitEpi)
  checkAll("Int > Byte", SplitEpiTests(ex2).splitEpi)
  checkAll("Long > Int > Byte", SplitEpiTests(ex1 composeSplitEpi ex2).splitEpi)

}
