// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb._
import gem.enum.{ Half, Site }
import java.time.{ Year, ZoneId }
import java.time.format.DateTimeFormatter
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SemesterSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbEnumerated._
  import ArbSemester._
  import ArbTime._

  ".fromString" should "be invertible via .format" in {
    forAll { (y: Year, h: Half) =>
      val yʹ = DateTimeFormatter.ofPattern("yyyy").format(y)
      val hʹ = h.tag
      val sʹ = s"$yʹ$hʹ"
      Semester.fromString(sʹ).map(_.format) shouldEqual Some(sʹ)
    }
  }

  ".unsafeFromString" should "be invertible via .format" in {
    forAll { (y: Year, h: Half) =>
      val yʹ = DateTimeFormatter.ofPattern("yyyy").format(y)
      val hʹ = h.tag
      val sʹ = s"$yʹ$hʹ"
      Semester.unsafeFromString(sʹ).format shouldEqual sʹ
    }
  }

  ".format" should "be invertible via .fromString" in {
    forAll { (s: Semester) =>
      Semester.fromString(s.format) shouldEqual Some(s)
    }
  }

  it should "also be invertible via .unsafeFromString" in {
    forAll { (s: Semester) =>
      Semester.unsafeFromString(s.format) shouldEqual s
    }
  }

  ".plusYears" should "result in a properly updated year" in {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      val sʹ = s.plusYears(nʹ)
      sʹ.year.getValue shouldEqual s.year.getValue + nʹ
    }
  }

  it should "never affect the half" in {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      val sʹ = s.plusYears(nʹ)
      sʹ.half shouldEqual s.half
    }
  }

  it should "have an identity" in {
    forAll { (s: Semester) =>
      s.plusYears(0) shouldEqual s
    }
  }

  it should "be invertible by negation, for reasonable values" in {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      s.plusYears(nʹ).plusYears(-nʹ) shouldEqual s
    }
  }

  it should "be a homomorphism over addition, for reasonable values" in {
    forAll { (s: Semester, n1: Short, n2: Short) =>
      val (n1ʹ, n2ʹ) = (n1.toInt, n2.toInt)
      s.plusYears(n1ʹ).plusYears(n2ʹ) shouldEqual s.plusYears(n1ʹ + n2ʹ)
    }
  }

  ".plusSemesters" should "be consistent with .plusYears, for reasonable values" in {
    forAll { (s: Semester, n: Byte, b: Boolean) =>
      val nʹ = n.toInt
      val bʹ = if (b) nʹ.signum else 0
      val s1 = s.plusYears(nʹ).plusSemesters(bʹ)
      val s2 = s.plusSemesters(nʹ * 2 + bʹ)
      s1 shouldEqual s2
    }
  }

  it should "have an identity" in {
    forAll { (s: Semester) =>
      s.plusSemesters(0) shouldEqual s
    }
  }

  it should "be invertible by negation, for reasonable values" in {
    forAll { (s: Semester, n: Byte) =>
      val nʹ = n.toInt
      s.plusSemesters(nʹ).plusSemesters(-nʹ) shouldEqual s
    }
  }

  it should "be a homomorphism over addition, for reasonable values" in {
    forAll { (s: Semester, n1: Byte, n2: Byte) =>
      val (n1ʹ, n2ʹ) = (n1.toInt, n2.toInt)
      s.plusSemesters(n1ʹ).plusSemesters(n2ʹ) shouldEqual s.plusSemesters(n1ʹ + n2ʹ)
    }
  }

  ".start round-tripping" should "be consistent for .yearMonth     ~ .fromYearMonth" in {
    forAll { (s: Semester) =>
      Semester.fromYearMonth(s.start.yearMonth) shouldEqual s
    }
  }

  it should "be consistent for .localDate     ~ .fromLocalDate" in {
    forAll { (s: Semester) =>
      Semester.fromLocalDate(s.start.localDate) shouldEqual s
    }
  }

  it should "be consistent for .localDateTime ~ .fromLocalDateTime" in {
    forAll { (s: Semester) =>
      Semester.fromLocalDateTime(s.start.localDateTime) shouldEqual s
    }
  }

  it should "be consistent for .zonedDateTime ~ .fromZonedDateTime" in {
    forAll { (s: Semester, z: ZoneId) =>
      Semester.fromZonedDateTime(s.start.zonedDateTime(z)) shouldEqual s
    }
  }

  it should "be consistent for .atSite        ~ .fromSiteAndInstant" in {
    forAll { (s: Semester, site: Site) =>
      Semester.fromSiteAndInstant(site, s.start.atSite(site).toInstant) shouldEqual s
    }
  }

  ".start precisiom" should "be correct for .yearMonth" in {
    forAll { (s: Semester) =>
      Semester.fromYearMonth(s.start.yearMonth.minusMonths(1)) shouldEqual s.prev
    }
  }

  it should "be correct for .localDate" in {
    forAll { (s: Semester) =>
      Semester.fromLocalDate(s.start.localDate.minusDays(1)) shouldEqual s.prev
    }
  }

  it should "be correct for .localDateTime" in {
    forAll { (s: Semester) =>
      Semester.fromLocalDateTime(s.start.localDateTime.minusNanos(1)) shouldEqual s.prev
    }
  }

  it should "be correct for .zonedDateTime" in {
    forAll { (s: Semester, z: ZoneId) =>
      Semester.fromZonedDateTime(s.start.zonedDateTime(z).minusNanos(1)) shouldEqual s.prev
    }
  }

  it should "be correct for .atSite" in {
    forAll { (s: Semester, site: Site) =>
      Semester.fromSiteAndInstant(site, s.start.atSite(site).minusNanos(1).toInstant) shouldEqual s.prev
    }
  }

  ".end round-tripping" should "be consistent for .yearMonth     ~ .fromYearMonth" in {
    forAll { (s: Semester) =>
      Semester.fromYearMonth(s.end.yearMonth) shouldEqual s
    }
  }

  it should "be consistent for .localDate     ~ .fromLocalDate" in {
    forAll { (s: Semester) =>
      Semester.fromLocalDate(s.end.localDate) shouldEqual s
    }
  }

  it should "be consistent for .localDateTime ~ .fromLocalDateTime" in {
    forAll { (s: Semester) =>
      Semester.fromLocalDateTime(s.end.localDateTime) shouldEqual s
    }
  }

  it should "be consistent for .zonedDateTime ~ .fromZonedDateTime" in {
    forAll { (s: Semester, z: ZoneId) =>
      Semester.fromZonedDateTime(s.end.zonedDateTime(z)) shouldEqual s
    }
  }

  it should "be consistent for .atSite        ~ .fromSiteAndInstant" in {
    forAll { (s: Semester, site: Site) =>
      Semester.fromSiteAndInstant(site, s.end.atSite(site).toInstant) shouldEqual s
    }
  }

  ".end precisiom" should "be correct for .yearMonth" in {
    forAll { (s: Semester) =>
      Semester.fromYearMonth(s.end.yearMonth.plusMonths(1)) shouldEqual s.next
    }
  }

  it should "be correct for .localDate" in {
    forAll { (s: Semester) =>
      Semester.fromLocalDate(s.end.localDate.plusDays(1)) shouldEqual s.next
    }
  }

  it should "be correct for .localDateTime" in {
    forAll { (s: Semester) =>
      Semester.fromLocalDateTime(s.end.localDateTime.plusNanos(1)) shouldEqual s.next
    }
  }

  it should "be correct for .zonedDateTime" in {
    forAll { (s: Semester, z: ZoneId) =>
      Semester.fromZonedDateTime(s.end.zonedDateTime(z).plusNanos(1)) shouldEqual s.next
    }
  }

  it should "be correct for .atSite" in {
    forAll { (s: Semester, site: Site) =>
      Semester.fromSiteAndInstant(site, s.end.atSite(site).plusNanos(1).toInstant) shouldEqual s.next
    }
  }

}
