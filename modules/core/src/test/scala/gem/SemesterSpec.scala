// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SemesterSpec extends FlatSpec with Matchers with PropertyChecks with Arbitraries {

  "Semester" should "reparse" in {
    forAll { (s: Semester) =>
      Semester.fromString(s.format) shouldEqual Some(s)
    }
  }

}
