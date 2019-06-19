// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb.{ ArbEnumerated, ArbObservingNight }
import gem.instances.time._
import gsp.math.arb.ArbTime
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import monocle.law.discipline._

final class LocalObservingNightSpec extends CatsSuite {
  import ArbTime._
  import ArbEnumerated._
  import ArbObservingNight._

  checkAll("LocalObservingNight", OrderTests[LocalObservingNight].order)
  checkAll("LocalObservingNight.localDate", IsoTests(LocalObservingNight.localDate))

  test("Equality must be natural") {
    forAll { (a: LocalObservingNight, b: LocalObservingNight) =>
      a.equals(b) shouldEqual Eq[LocalObservingNight].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (o: LocalObservingNight) =>
      o.toString shouldEqual Show[LocalObservingNight].show(o)
    }
  }

  test("Always begins at 2PM") {
    forAll { (o: LocalObservingNight) =>
      o.start.getHour shouldEqual LocalObservingNight.StartHour
    }
  }

  test("Always ends at 2PM") {
    forAll { (o: LocalObservingNight) =>
      o.end.getHour shouldEqual LocalObservingNight.StartHour
    }
  }

  test("Is contiguous (1)") {
    forAll { (o: LocalObservingNight) =>
      o.previous.end shouldEqual o.start
    }
  }

  test("Is contiguous (2)") {
    forAll { (o: LocalObservingNight) =>
      o.next.start shouldEqual o.end
    }
  }

  test("Includes start") {
    forAll { (o: LocalObservingNight) =>
      o.includes(o.start) shouldBe true
    }
  }

  test("Excludes end") {
    forAll { (o: LocalObservingNight) =>
      o.includes(o.end) shouldBe false
    }
  }

  test("night.previous.next shouldEqual night") {
    forAll { (o: LocalObservingNight) =>
      o.previous.next shouldEqual o
    }
  }

  test("can always parse a formatted night") {
    forAll { (o: LocalObservingNight) =>
      LocalObservingNight.fromString(o.format) shouldEqual Some(o)
    }
  }

}
