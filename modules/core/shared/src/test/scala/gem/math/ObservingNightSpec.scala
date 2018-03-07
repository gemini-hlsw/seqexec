// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb._
import gem.enum.Site
import gem.math.ObservingNight.LocalNightStartHour

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite

import java.time._


@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class ObservingNightSpec extends CatsSuite {

  import ArbObservingNight._

  checkAll("ObservingNight", OrderTests[ObservingNight].order)

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

  test("Always begins at 2PM") {
    forAll { (o: ObservingNight) =>
      o.start.atZone(o.site.timezone).getHour shouldEqual LocalNightStartHour
    }
  }

  test("Always ends at 2PM") {
    forAll { (o: ObservingNight) =>
      o.end.atZone(o.site.timezone).getHour shouldEqual LocalNightStartHour
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
    val o = ObservingNight.forYMD(Year.of(2018), Month.MAY, 13, Site.GS)
    o.map(_.duration.toHours) shouldEqual Some(25)
  }

  test("handle daylight savings correctly (winter end)") {
    val o = ObservingNight.forYMD(Year.of(2018), Month.AUGUST, 12, Site.GS)
    o.map(_.duration.toHours) shouldEqual Some(23)
  }

  test("can always parse a formatted night") {
    forAll { (o: ObservingNight) =>
      ObservingNight.parse(o.format, o.site) shouldEqual Some(o)
    }
  }
}
