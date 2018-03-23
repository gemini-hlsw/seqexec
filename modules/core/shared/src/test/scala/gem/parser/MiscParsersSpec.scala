// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.parser

import atto._, Atto._
import cats.tests.CatsSuite
import gem.math.Index
import gem.parser.MiscParsers.index

final class MiscParsersSpec extends CatsSuite {

  test("index parser must be consistent with Index.fromShort") {
    forAll { (s: Short) =>
      index.parseOnly(s.toString).option shouldEqual Index.fromShort.getOption(s)
    }
  }

}
