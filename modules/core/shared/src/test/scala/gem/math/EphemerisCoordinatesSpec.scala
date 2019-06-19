// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb.ArbEphemeris
import gsp.math.{ Angle, Offset }
import gsp.math.arb.{ ArbCoordinates, ArbDeclination, ArbOffset, ArbRightAscension }

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import monocle.law.discipline._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.Assertion

final class EphemerisCoordinatesSpec extends CatsSuite {
  import ArbCoordinates._
  import ArbEphemeris._
  import ArbOffset._
  import ArbRightAscension._
  import ArbDeclination._
  import EphemerisCoordinatesSpec._

  // Laws
  checkAll("EphemerisCoordinates", EqTests[EphemerisCoordinates].eqv)
  checkAll("EphemerisCoordinates.coordinates", LensTests(EphemerisCoordinates.coordinates))
  checkAll("EphemerisCoordinates.rightAscension", LensTests(EphemerisCoordinates.rightAscension))
  checkAll("EphemerisCoordinates.declination", LensTests(EphemerisCoordinates.declination))
  checkAll("EphemerisCoordinates.delta", LensTests(EphemerisCoordinates.delta))
  checkAll("EphemerisCoordinates.deltaP", LensTests(EphemerisCoordinates.deltaP))
  checkAll("EphemerisCoordinates.deltaQ", LensTests(EphemerisCoordinates.deltaQ))

  test("Equality must be natural") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      a.equals(b) shouldEqual Eq[EphemerisCoordinates].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: EphemerisCoordinates) =>
      a.toString shouldEqual Show[EphemerisCoordinates].show(a)
    }
  }

  test("interpolate must be consistent with Coordinates.interpolate") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates, r: Ratio) =>
      a.interpolate(b, r.ratio).coord shouldEqual a.coord.interpolate(b.coord, r.ratio)
    }
  }

  test("interpolate velocity between any point and itself must yield the same velocity") {
    forAll { (a: EphemerisCoordinates, r: Ratio) =>
      a.interpolate(a, r.ratio).delta shouldEqual a.delta
    }
  }

  test("interpolate velocity with factor 0 should yield the first point") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      a.interpolate(b, 0.0).delta shouldEqual a.delta
    }
  }

  test("interpolate velocity with factor 1 should yield the second point") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      a.interpolate(b, 1.0).delta shouldEqual b.delta
    }
  }

  private def midpoint(a: EphemerisCoordinates, b: EphemerisCoordinates, f: Offset => Angle): Assertion = {
    val m  = a.interpolate(b, 0.5).delta
    val mΔ = Angle.signedMicroarcseconds.get(f(m))

    val aΔ = Angle.signedMicroarcseconds.get(f(a.delta))
    val bΔ = Angle.signedMicroarcseconds.get(f(b.delta))

    mΔ shouldEqual ((aΔ + bΔ)/2.0).round
  }

  test("interpolate velocity with factor 0.5 should yield the midpoint p") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      midpoint(a, b, _.p.toAngle)
    }
  }

  test("interpolate velocity with factor 0.5 should yield the midpoint q") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      midpoint(a, b, _.q.toAngle)
    }
  }

}

object EphemerisCoordinatesSpec {

  final case class Ratio(ratio: Double)

  implicit val arbRatio: Arbitrary[Ratio] =
    Arbitrary {
      Gen.choose(0.0, 1.0).map(Ratio(_))
    }

}
