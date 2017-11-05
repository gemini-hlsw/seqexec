// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import gem.arb._
import gem.enum.{ Site, DailyProgramType }
import java.time._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class ProgramIdSpec extends CatsSuite {
  import ProgramId._
  import ArbEnumerated._
  import ArbProgramId._
  import ArbTime._

  // Laws
  checkAll("Program.Id", OrderTests[Program.Id].order)

  test("Equality must be natural") {
    forAll { (a: ProgramId, b: ProgramId) =>
      a.equals(b) shouldEqual Eq[ProgramId].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: ProgramId) =>
      a.toString shouldEqual Show[ProgramId].show(a)
    }
  }

  test("Science must reparse") {
    forAll { (sid: Science) =>
      Science.fromString(sid.format) shouldEqual Some(sid)
    }
  }

  test("Science should never reparse into a Nonstandard, even if we try") {
    forAll { (sid: Science) =>
      Nonstandard.fromString(sid.format) shouldEqual None
    }
  }

  test("Daily must reparse") {
    forAll { (did: Daily) =>
      Daily.fromString(did.format) shouldEqual Some(did)
    }
  }

  test("Daily should never reparse into a Nonstandard, even if we try") {
    forAll { (did: Science) =>
      Daily.fromString(did.format) shouldEqual None
    }
  }

  test("Daily should find the correct observing day given a site and instant") {
    forAll { (site: Site, ldt: LocalDateTime, dpt: DailyProgramType) =>
      val zdt = ZonedDateTime.of(ldt, site.timezone)
      val did = Daily.fromSiteAndInstant(site, zdt.toInstant, dpt)
      val end = zdt.`with`(LocalTime.of(14, 0, 0, 0))
      val exp = if (zdt isBefore end) ldt.toLocalDate else ldt.plusDays(1).toLocalDate
      did.localDate shouldEqual exp
    }
  }

  test("Daily should be consistent re: .start and .fromSiteAndInstant") {
    forAll { (did: Daily) =>
      Daily.fromSiteAndInstant(did.site, did.start.toInstant, did.dailyProgramType) shouldEqual did
    }
  }

  test("Daily should be consistent re: .end and .fromSiteAndInstant") {
    forAll { (did: Daily) =>
      Daily.fromSiteAndInstant(did.site, did.end.toInstant, did.dailyProgramType) shouldEqual did
    }
  }

  test("Daily should have a consistent program type and daily program type") {
    forAll { (did: Daily) =>
      did.dailyProgramType.toProgramType shouldEqual did.programType
    }
  }

  test("Daily should have a consistent date and semester") {
    forAll { (did: Daily) =>
      Semester.fromLocalDate(did.localDate) shouldEqual did.semester
    }
  }

  test("Nonstandard must reparse") {
    forAll { (nid: Nonstandard) =>
      Nonstandard.fromString(nid.format) shouldEqual Some(nid)
    }
  }

  test("ProgramId must reparse") {
    forAll { (pid: ProgramId) =>
      ProgramId.fromString(pid.format) shouldEqual Some(pid)
    }
  }

}
