// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package optics

import cats.tests.CatsSuite
import gem.laws.discipline._

final class SplitMonoSpec extends CatsSuite {

  // Our example SplitMono injects Byte from Int
  val example: SplitMono[Byte, Int] =
    SplitMono(_.toInt, _.toByte)

  // Ensure it's lawful
  checkAll("ByteInt", SplitMonoTests(example).splitMono)

}
