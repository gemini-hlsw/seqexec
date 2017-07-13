// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb._
import java.time.Instant
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }
import scalaz.{ Equal, Order, Show }
import scalaz.std.string._

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
class DatasetSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbDataset._

  // Compilation test
  protected val a1 = implicitly[Order[Dataset]]
  protected val a2 = implicitly[Show[Dataset]]

  "Equality" must "be natural" in {
    forAll { (a: Dataset, b: Dataset) =>
      a.equals(b) shouldEqual Equal[Dataset].equal(a, b)
    }
  }

  it must "operate pairwise" in {
    forAll { (a: Dataset, b: Dataset) =>
      Equal[Dataset.Label].equal(a.label, b.label) &&
      Equal[String].equal(a.filename, b.filename)  &&
      Equal[Instant].equal(a.timestamp, b.timestamp) shouldEqual Equal[Dataset].equal(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Dataset) =>
      a.toString shouldEqual Show[Dataset].shows(a)
    }
  }

}
