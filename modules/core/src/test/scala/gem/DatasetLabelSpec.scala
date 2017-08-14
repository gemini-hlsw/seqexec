// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Eq, Order, Show }
import cats.implicits._
import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
class DatasetLabelSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbDataset._

  // Compilation test
  protected val a1 = implicitly[Order[Dataset.Label]]
  protected val a2 = implicitly[Show[Dataset.Label]]

  "Equality" must "be natural" in {
    forAll { (a: Dataset.Label, b: Dataset.Label) =>
      a.equals(b) shouldEqual Eq[Dataset.Label].eqv(a, b)
    }
  }

  it must "operate pairwise" in {
    forAll { (a: Dataset.Label, b: Dataset.Label) =>
      Eq[Observation.Id].eqv(a.observationId, b.observationId) &&
      Eq[Int].eqv(a.index, b.index) shouldEqual Eq[Dataset.Label].eqv(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Dataset.Label) =>
      a.toString shouldEqual Show[Dataset.Label].show(a)
    }
  }

  ".format" should "reparse" in {
    forAll { (a: Dataset.Label) =>
      Dataset.Label.fromString(a.format) shouldEqual Some(a)
    }
  }

}
