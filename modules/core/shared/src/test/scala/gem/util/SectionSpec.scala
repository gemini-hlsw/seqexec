// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package util

import cats.tests.CatsSuite
import gem.laws.discipline._

final class SectionSpec extends CatsSuite {

  // Our example Section injects Int into Byte
  val example: Section[Int, Byte] =
    Section(_.toByte, _.toInt)

  // Ensure it's lawful
  checkAll("Sections.HmsDms", SectionTests(example).section)

}
