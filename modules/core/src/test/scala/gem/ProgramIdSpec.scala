// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Eq, Show }
import gem.arb._
import gem.enum.{ Site, DailyProgramType }
import java.time._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class ProgramIdSpec extends FlatSpec with Matchers with PropertyChecks {
  import ProgramId._
  import ArbEnumerated._
  import ArbProgramId._
  import ArbTime._

  "Equality" must "be natural" in {
    forAll { (a: ProgramId, b: ProgramId) =>
      a.equals(b) shouldEqual Eq[ProgramId].eqv(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: ProgramId) =>
      a.toString shouldEqual Show[ProgramId].show(a)
    }
  }

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

  it should "find the correct observing day given a site and instant" in {
    forAll { (site: Site, ldt: LocalDateTime, dpt: DailyProgramType) =>
      val zdt = ZonedDateTime.of(ldt, site.timezone)
      val did = Daily.fromSiteAndInstant(site, zdt.toInstant, dpt)
      val end = zdt.`with`(LocalTime.of(14, 0, 0, 0))
      val exp = if (zdt isBefore end) ldt.toLocalDate else ldt.plusDays(1).toLocalDate
      did.localDate shouldEqual exp
    }
  }

  it should "be consistent re: .start and .fromSiteAndInstant" in {
    forAll { (did: Daily) =>
      Daily.fromSiteAndInstant(did.site, did.start.toInstant, did.dailyProgramType) shouldEqual did
    }
  }

  it should "be consistent re: .end and .fromSiteAndInstant" in {
    forAll { (did: Daily) =>
      Daily.fromSiteAndInstant(did.site, did.end.toInstant, did.dailyProgramType) shouldEqual did
    }
  }

  it should "have a consistent program type and daily program type" in {
    forAll { (did: Daily) =>
      did.dailyProgramType.toProgramType shouldEqual did.programType
    }
  }

  it should "have a consistent date and semester" in {
    forAll { (did: Daily) =>
      Semester.fromLocalDate(did.localDate) shouldEqual did.semester
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
