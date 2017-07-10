// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

class ProgramIdSpec extends FlatSpec with Matchers with PropertyChecks {
  import ProgramId._
  import ArbProgramId._

  "Science" should "reparse" in {
    forAll { (sid: Science) =>
      Science.fromString(sid.format) shouldEqual Some(sid)
    }
  }

  it should "never reparse into a Nonstandard, even if we try" in {
    forAll { (sid: Science) =>
      Nonstandard.fromString(sid.format) shouldEqual None
    }
  }

  "Daily" should "reparse" in {
    forAll { (did: Daily) =>
      Daily.fromString(did.format) shouldEqual Some(did)
    }
  }

  it should "never reparse into a Nonstandard, even if we try" in {
    forAll { (did: Science) =>
      Daily.fromString(did.format) shouldEqual None
    }
  }

  "Nonstandard" should "reparse" in {
    forAll { (nid: Nonstandard) =>
      Nonstandard.fromString(nid.format) shouldEqual Some(nid)
    }
  }

  "ProgramId" should "reparse" in {
    forAll { (pid: ProgramId) =>
      ProgramId.fromString(pid.format) shouldEqual Some(pid)
    }
  }

}
