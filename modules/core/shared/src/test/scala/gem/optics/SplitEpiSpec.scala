// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package optics

import cats.tests.CatsSuite
import gem.laws.discipline._

final class SplitEpiSpec extends CatsSuite {

  // Our example SplitEpi injects Int into Byte
  val example: SplitEpi[Int, Byte] =
    SplitEpi(_.toByte, _.toInt)

  // Ensure it's lawful
  checkAll("IntByte", SplitEpiTests(example).splitEpi)

}
