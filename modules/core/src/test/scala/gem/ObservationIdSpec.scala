// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }
import scalaz.{ Equal, Show }
import scalaz.std.anyVal._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class ObservationIdSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbObservation._

  "Equality" must "be natural" in {
    forAll { (a: Observation.Id, b: Observation.Id) =>
      a.equals(b) shouldEqual Equal[Observation.Id].equal(a, b)
    }
  }

  it must "act pairwise" in {
    forAll { (a: Observation.Id, b: Observation.Id) =>
      Equal[Program.Id].equal(a.pid, b.pid) &&
      Equal[Int].equal(a.index, b.index) shouldEqual Equal[Observation.Id].equal(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Observation.Id) =>
      a.toString shouldEqual Show[Observation.Id].shows(a)
    }
  }

  ".format" should "reparse" in {
    forAll { (a: Observation.Id) =>
      Observation.Id.fromString(a.format) shouldEqual Some(a)
    }
  }

}
