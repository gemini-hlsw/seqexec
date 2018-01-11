// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gem.arb._
import gem.laws.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
final class DatasetLabelSpec extends CatsSuite {
  import ArbDataset._

  // Laws
  checkAll("DatasetLabel", OrderTests[Dataset.Label].order)
  checkAll("Optics.fromString", FormatTests(Dataset.Label.Optics.fromString).formatWith(strings))

  test("Equality must be natural") {
    forAll { (a: Dataset.Label, b: Dataset.Label) =>
      a.equals(b) shouldEqual Eq[Dataset.Label].eqv(a, b)
    }
  }

  test("Equality must operate pairwise") {
    forAll { (a: Dataset.Label, b: Dataset.Label) =>
      Eq[Observation.Id].eqv(a.observationId, b.observationId) &&
      Eq[Int].eqv(a.index, b.index) shouldEqual Eq[Dataset.Label].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Dataset.Label) =>
      a.toString shouldEqual Show[Dataset.Label].show(a)
    }
  }

}
