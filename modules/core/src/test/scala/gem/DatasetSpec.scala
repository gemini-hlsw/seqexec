// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Eq, Order, Show }
import cats.implicits._
import gem.arb._
import gem.imp.TimeInstances._
import java.time.Instant
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
class DatasetSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbDataset._

  // Compilation test
  protected val a1 = implicitly[Order[Dataset]]
  protected val a2 = implicitly[Show[Dataset]]

  "Equality" must "be natural" in {
    forAll { (a: Dataset, b: Dataset) =>
      a.equals(b) shouldEqual Eq[Dataset].eqv(a, b)
    }
  }

  it must "operate pairwise" in {
    forAll { (a: Dataset, b: Dataset) =>
      Eq[Dataset.Label].eqv(a.label, b.label) &&
      Eq[String].eqv(a.filename, b.filename)  &&
      Eq[Instant].eqv(a.timestamp, b.timestamp) shouldEqual Eq[Dataset].eqv(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Dataset) =>
      a.toString shouldEqual Show[Dataset].show(a)
    }
  }

}
