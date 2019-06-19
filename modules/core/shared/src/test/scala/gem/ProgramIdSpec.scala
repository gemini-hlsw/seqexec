// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import gem.arb._
import gem.enum.{ Site, DailyProgramType }
import gsp.math.arb.ArbTime
import java.time._
import monocle.law.discipline._

final class ProgramIdSpec extends CatsSuite {
  import ProgramId._
  import ArbEnumerated._
  import ArbProgramId._
  import ArbTime._

  // Laws
  checkAll("Program.Id", OrderTests[Program.Id].order)
  checkAll("Program.Id.Science.fromString", PrismTests(Program.Id.Science.fromString))
  checkAll("Program.Id.Daily.fromString", PrismTests(Program.Id.Daily.fromString))
  checkAll("Program.Id.Nonstandard.fromString", PrismTests(Program.Id.Nonstandard.fromString))
  checkAll("Program.Id.fromString", PrismTests(Program.Id.fromString))

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

  test("Science should never reparse into a Nonstandard, even if we try") {
    forAll { (sid: Science) =>
      Nonstandard.fromString.getOption(ProgramId.fromString.reverseGet(sid)) shouldEqual None
    }
  }

  test("Daily should never reparse into a Nonstandard, even if we try") {
    forAll { (did: Daily) =>
      Nonstandard.fromString.getOption(ProgramId.fromString.reverseGet(did)) shouldEqual None
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

}
