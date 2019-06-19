// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import gem.arb._
import gem.enum.{ Half, Site }
import gem.instances.time._
import gsp.math.arb.ArbTime
import java.time.{ Year, ZoneId }
import java.time.format.DateTimeFormatter

final class SemesterSpec extends CatsSuite {
  import ArbEnumerated._
  import ArbSemester._
  import ArbTime._

  // Laws
  checkAll("Semester", OrderTests[Semester].order)

  test("Equality must be natural") {
    forAll { (a: Semester, b: Semester) =>
      a.equals(b) shouldEqual Eq[Semester].eqv(a, b)
    }
  }

  test("Equality must operate pairwise") {
    forAll { (a: Semester, b: Semester) =>
      Eq[Year].eqv(a.year, b.year) &&
      Eq[Half].eqv(a.half, b.half) shouldEqual Eq[Semester].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Semester) =>
      a.toString shouldEqual Show[Semester].show(a)
    }
  }

  test(".fromString must be invertible via .format") {
    forAll { (y: Year, h: Half) =>
      val yʹ = DateTimeFormatter.ofPattern("yyyy").format(y)
      val hʹ = h.tag
      val sʹ = s"$yʹ$hʹ"
      Semester.fromString(sʹ).map(_.format) shouldEqual Some(sʹ)
    }
  }

  test(".unsafeFromString must be invertible via .format") {
    forAll { (y: Year, h: Half) =>
      val yʹ = DateTimeFormatter.ofPattern("yyyy").format(y)
      val hʹ = h.tag
      val sʹ = s"$yʹ$hʹ"
      Semester.unsafeFromString(sʹ).format shouldEqual sʹ
    }
  }

  test(".format must be invertible via .fromString") {
    forAll { (s: Semester) =>
      Semester.fromString(s.format) shouldEqual Some(s)
    }
  }

  test(".format should also be invertible via .unsafeFromString") {
    forAll { (s: Semester) =>
      Semester.unsafeFromString(s.format) shouldEqual s
    }
  }

  test(".plusYears must result in a properly updated year") {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      val sʹ = s.plusYears(nʹ)
      sʹ.year.getValue shouldEqual s.year.getValue + nʹ
    }
  }

  test(".plusYears should never affect the half") {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      val sʹ = s.plusYears(nʹ)
      sʹ.half shouldEqual s.half
    }
  }

  test(".plusYears should have an identity") {
    forAll { (s: Semester) =>
      s.plusYears(0) shouldEqual s
    }
  }

  test(".plusYears should be invertible by negation, for reasonable values") {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      s.plusYears(nʹ).plusYears(-nʹ) shouldEqual s
    }
  }

  test(".plusYears should be a homomorphism over addition, for reasonable values") {
    forAll { (s: Semester, n1: Short, n2: Short) =>
      val (n1ʹ, n2ʹ) = (n1.toInt, n2.toInt)
      s.plusYears(n1ʹ).plusYears(n2ʹ) shouldEqual s.plusYears(n1ʹ + n2ʹ)
    }
  }

  test(".plusSemesters must be consistent with .plusYears, for reasonable values") {
    forAll { (s: Semester, n: Byte, b: Boolean) =>
      val nʹ = n.toInt
      val bʹ = if (b) nʹ.signum else 0
      val s1 = s.plusYears(nʹ).plusSemesters(bʹ)
      val s2 = s.plusSemesters(nʹ * 2 + bʹ)
      s1 shouldEqual s2
    }
  }

  test(".plusSemesters should have an identity") {
    forAll { (s: Semester) =>
      s.plusSemesters(0) shouldEqual s
    }
  }

  test(".plusSemesters should be invertible by negation, for reasonable values") {
    forAll { (s: Semester, n: Byte) =>
      val nʹ = n.toInt
      s.plusSemesters(nʹ).plusSemesters(-nʹ) shouldEqual s
    }
  }

  test(".plusSemesters should be a homomorphism over addition, for reasonable values") {
    forAll { (s: Semester, n1: Byte, n2: Byte) =>
      val (n1ʹ, n2ʹ) = (n1.toInt, n2.toInt)
      s.plusSemesters(n1ʹ).plusSemesters(n2ʹ) shouldEqual s.plusSemesters(n1ʹ + n2ʹ)
    }
  }

  test(".start round-tripping must be consistent for .yearMonth     ~ .fromYearMonth") {
    forAll { (s: Semester) =>
      Semester.fromYearMonth(s.start.yearMonth) shouldEqual s
    }
  }

  test(".start should be consistent for .localDate     ~ .fromLocalDate") {
    forAll { (s: Semester) =>
      Semester.fromLocalDate(s.start.localDate) shouldEqual s
    }
  }

  test(".start should be consistent for .localDateTime ~ .fromLocalDateTime") {
    forAll { (s: Semester) =>
      Semester.fromLocalDateTime(s.start.localDateTime) shouldEqual s
    }
  }

  test(".start should be consistent for .zonedDateTime ~ .fromZonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      Semester.fromZonedDateTime(s.start.zonedDateTime(z)) shouldEqual s
    }
  }

  test(".start should be consistent for .atSite        ~ .fromSiteAndInstant") {
    forAll { (s: Semester, site: Site) =>
      Semester.fromSiteAndInstant(site, s.start.atSite(site).toInstant) shouldEqual s
    }
  }

  test(".start precision must be correct for .yearMonth") {
    forAll { (s: Semester) =>
      Semester.fromYearMonth(s.start.yearMonth.minusMonths(1)) shouldEqual s.prev
    }
  }

  test(".start should be correct for .localDate") {
    forAll { (s: Semester) =>
      Semester.fromLocalDate(s.start.localDate.minusDays(1)) shouldEqual s.prev
    }
  }

  test(".start should be correct for .localDateTime") {
    forAll { (s: Semester) =>
      Semester.fromLocalDateTime(s.start.localDateTime.minusNanos(1)) shouldEqual s.prev
    }
  }

  test(".start should be correct for .zonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      Semester.fromZonedDateTime(s.start.zonedDateTime(z).minusNanos(1)) shouldEqual s.prev
    }
  }

  test(".start should be correct for .atSite") {
    forAll { (s: Semester, site: Site) =>
      Semester.fromSiteAndInstant(site, s.start.atSite(site).minusNanos(1).toInstant) shouldEqual s.prev
    }
  }

  test(".end round-tripping must be consistent for .yearMonth     ~ .fromYearMonth") {
    forAll { (s: Semester) =>
      Semester.fromYearMonth(s.end.yearMonth) shouldEqual s
    }
  }

  test(".end should be consistent for .localDate     ~ .fromLocalDate") {
    forAll { (s: Semester) =>
      Semester.fromLocalDate(s.end.localDate) shouldEqual s
    }
  }

  test(".end should be consistent for .localDateTime ~ .fromLocalDateTime") {
    forAll { (s: Semester) =>
      Semester.fromLocalDateTime(s.end.localDateTime) shouldEqual s
    }
  }

  test(".end should be consistent for .zonedDateTime ~ .fromZonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      Semester.fromZonedDateTime(s.end.zonedDateTime(z)) shouldEqual s
    }
  }

  test(".end should be consistent for .atSite        ~ .fromSiteAndInstant") {
    forAll { (s: Semester, site: Site) =>
      Semester.fromSiteAndInstant(site, s.end.atSite(site).toInstant) shouldEqual s
    }
  }

  test(".end precisiom must be correct for .yearMonth") {
    forAll { (s: Semester) =>
      Semester.fromYearMonth(s.end.yearMonth.plusMonths(1)) shouldEqual s.next
    }
  }

  test(".end should be correct for .localDate") {
    forAll { (s: Semester) =>
      Semester.fromLocalDate(s.end.localDate.plusDays(1)) shouldEqual s.next
    }
  }

  test(".end should be correct for .localDateTime") {
    forAll { (s: Semester) =>
      Semester.fromLocalDateTime(s.end.localDateTime.plusNanos(1)) shouldEqual s.next
    }
  }

  test(".end should be correct for .zonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      Semester.fromZonedDateTime(s.end.zonedDateTime(z).plusNanos(1)) shouldEqual s.next
    }
  }

  test(".end should be correct for .atSite") {
    forAll { (s: Semester, site: Site) =>
      Semester.fromSiteAndInstant(site, s.end.atSite(site).plusNanos(1).toInstant) shouldEqual s.next
    }
  }

}
