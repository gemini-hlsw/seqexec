// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

import scalaz.{ Equal, Show }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class CoordinatesSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbCoordinates._
  import ArbAngle._

  "Equality" must "be natural" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      a.equals(b) shouldEqual Equal[Coordinates].equal(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Coordinates) =>
      a.toString shouldEqual Show[Coordinates].shows(a)
    }
  }

  "offsetWithCarry" must "be consistent with offset" in {
    forAll { (a: Coordinates, dRA: HourAngle, dDec: Angle) =>
      a.offset(dRA, dDec) shouldEqual a.offsetWithCarry(dRA, dDec)._1
    }
  }

  it must "be invertable" in {
    forAll { (a: Coordinates, dRA: HourAngle, dDec: Angle) =>
      a.offsetWithCarry(dRA, dDec) match {
        case (cs, false) => cs.offset(-dRA, -dDec) shouldEqual a
        case (cs, true)  => cs.offset(-dRA,  dDec) shouldEqual a
      }
    }
  }

  "diff" must "be consistent with offset" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      val (dRA, dDec) = a diff b
      a.offset(dRA, dDec) shouldEqual b
    }
  }

  it must "be consistent with offsetWithCarry, and never carry" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      val (dRA, dDec) = a diff b
      a.offsetWithCarry(dRA, dDec).shouldEqual((b, false))
    }
  }

}
