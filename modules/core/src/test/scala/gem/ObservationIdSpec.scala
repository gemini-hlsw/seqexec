// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Eq, Show }
import cats.implicits._
import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class ObservationIdSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbObservation._

  "Equality" must "be natural" in {
    forAll { (a: Observation.Id, b: Observation.Id) =>
      a.equals(b) shouldEqual Eq[Observation.Id].eqv(a, b)
    }
  }

  it must "act pairwise" in {
    forAll { (a: Observation.Id, b: Observation.Id) =>
      Eq[Program.Id].eqv(a.pid, b.pid) &&
      Eq[Int].eqv(a.index, b.index) shouldEqual Eq[Observation.Id].eqv(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Observation.Id) =>
      a.toString shouldEqual Show[Observation.Id].show(a)
    }
  }

  ".format" should "reparse" in {
    forAll { (a: Observation.Id) =>
      Observation.Id.fromString(a.format) shouldEqual Some(a)
    }
  }

}
