// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }
import scalaz.{ Equal, Order, Show }
import scalaz.std.anyVal._

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
class DatasetLabelSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbDataset._

  // Compilation test
  protected val a1 = implicitly[Order[Dataset.Label]]
  protected val a2 = implicitly[Show[Dataset.Label]]

  "Equality" must "be natural" in {
    forAll { (a: Dataset.Label, b: Dataset.Label) =>
      a.equals(b) shouldEqual Equal[Dataset.Label].equal(a, b)
    }
  }

  it must "operate pairwise" in {
    forAll { (a: Dataset.Label, b: Dataset.Label) =>
      Equal[Observation.Id].equal(a.observationId, b.observationId) &&
      Equal[Int].equal(a.index, b.index) shouldEqual Equal[Dataset.Label].equal(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Dataset.Label) =>
      a.toString shouldEqual Show[Dataset.Label].shows(a)
    }
  }

  ".format" should "reparse" in {
    forAll { (a: Dataset.Label) =>
      Dataset.Label.fromString(a.format) shouldEqual Some(a)
    }
  }

}
