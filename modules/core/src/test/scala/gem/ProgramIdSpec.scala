// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ProgramIdSpec extends FlatSpec with Matchers with PropertyChecks with Arbitraries {
  import ProgramId._

  "Science" should "reparse" in {
    forAll { (s: Science) =>
      Science.fromString(s.format) shouldEqual Some(s)
    }
  }

  "Daily" should "reparse" in {
    forAll { (s: Daily) =>
      Daily.fromString(s.format) shouldEqual Some(s)
    }
  }

}
