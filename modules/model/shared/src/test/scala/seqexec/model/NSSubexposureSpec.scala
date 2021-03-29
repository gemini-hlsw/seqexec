// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.tests.CatsSuite
import org.scalacheck.Gen

final class NSSubexposureSpec extends CatsSuite {
  test("subexposures calculations") {
    forAll(Gen.posNum[Int]) { n =>
      assert(NSSubexposure.subexposures(n).length === n * 4)
    }
  }
}
