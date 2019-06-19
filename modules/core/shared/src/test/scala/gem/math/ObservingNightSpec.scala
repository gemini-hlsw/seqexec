// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb.{ ArbEnumerated, ArbObservingNight }
import gem.enum.Site
import gem.instances.time._
import gsp.math.arb.ArbTime

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import java.time._
import monocle.law.discipline._


final class ObservingNightSpec extends CatsSuite {
  import ArbEnumerated._
  import ArbObservingNight._
  import ArbTime._

  checkAll("ObservingNight", OrderTests[ObservingNight].order)
  checkAll("ObservingNight.site", LensTests(ObservingNight.site))
  checkAll("ObservingNight.localObservingNight", LensTests(ObservingNight.localObservingNight))
  checkAll("ObservingNight.localDate", LensTests(ObservingNight.localDate))

  test("Equality must be natural") {
    forAll { (a: ObservingNight, b: ObservingNight) =>
      a.equals(b) shouldEqual Eq[ObservingNight].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (o: ObservingNight) =>
      o.toString shouldEqual Show[ObservingNight].show(o)
    }
  }

  test("Start time consistent with LocalObservingNight") {
    forAll { (o: ObservingNight) =>
      o.start.atZone(o.site.timezone).toLocalDateTime shouldEqual o.toLocalObservingNight.start
    }
  }

  test("End time consistent with LocalObservingNight") {
    forAll { (o: ObservingNight) =>
      o.end.atZone(o.site.timezone).toLocalDateTime shouldEqual o.toLocalObservingNight.end
    }
  }

  test("Is contiguous (1)") {
    forAll { (o: ObservingNight) =>
      o.previous.end shouldEqual o.start
    }
  }

  test("Is contiguous (2)") {
    forAll { (o: ObservingNight) =>
      o.next.start shouldEqual o.end
    }
  }

  test("Includes start") {
    forAll { (o: ObservingNight) =>
      o.includes(o.start) shouldBe true
    }
  }

  test("Excludes end") {
    forAll { (o: ObservingNight) =>
      o.includes(o.end) shouldBe false
    }
  }

  test("night.previous.next shouldEqual night") {
    forAll { (o: ObservingNight) =>
      o.previous.next shouldEqual o
    }
  }

  test("handle daylight savings correctly (summer end)") {
    val h = LocalObservingNight.fromString("20180513").map(_.atSite(Site.GS).duration.toHours)
    h shouldEqual Some(25)
  }

  test("handle daylight savings correctly (winter end)") {
    val h = LocalObservingNight.fromString("20180812").map(_.atSite(Site.GS).duration.toHours)
    h shouldEqual Some(23)
  }

  test("fromSiteAndLocalDate consistent") {
    forAll { (s: Site, l: LocalDate) =>
      ObservingNight.fromSiteAndLocalDate(s, l).toLocalDate shouldEqual l
    }
  }

  test("fromSiteAndLocalDateTime consistent") {
    forAll { (s: Site, l: LocalDateTime) =>
      val n  = ObservingNight.fromSiteAndLocalDateTime(s, l)
      val d  = l.toLocalDate
      val dʹ = if (l.toLocalTime.isBefore(LocalObservingNight.Start)) d else d.plusDays(1L)
      n.toLocalDate shouldEqual dʹ
    }
  }
}
