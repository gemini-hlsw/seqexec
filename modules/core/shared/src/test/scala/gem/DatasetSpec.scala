// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gem.arb._
import gem.instances.time._
import java.time.Instant

final class DatasetSpec extends CatsSuite {
  import ArbDataset._

  // Laws
  checkAll("DatasetLabel", OrderTests[Dataset].order)

  test("Equality must be natural") {
    forAll { (a: Dataset, b: Dataset) =>
      a.equals(b) shouldEqual Eq[Dataset].eqv(a, b)
    }
  }

  test("Equality must operate pairwise") {
    forAll { (a: Dataset, b: Dataset) =>
      Eq[Dataset.Label].eqv(a.label, b.label) &&
      Eq[String].eqv(a.filename, b.filename)  &&
      Eq[Instant].eqv(a.timestamp, b.timestamp) shouldEqual Eq[Dataset].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Dataset) =>
      a.toString shouldEqual Show[Dataset].show(a)
    }
  }

}
