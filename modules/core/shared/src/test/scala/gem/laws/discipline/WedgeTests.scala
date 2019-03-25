// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.laws
package discipline

import cats.Eq
import gem.optics.Wedge
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait WedgeTests[A, B] extends Laws {
  val laws: WedgeLaws[A, B]

  def wedge(
    implicit aa: Arbitrary[A], ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("Wedge",
      "normalize A"                     -> forAll((a: A) => laws.normalizeA(a)),
      "normalize B"                     -> forAll((b: B) => laws.normalizeB(b)),
      "reverseGet reverseGet roundtrip" -> forAll((a: A) => laws.normalizedGetRoundTrip(a)),
      "normalized get roundtrip"        -> forAll((b: B) => laws.normalizedReverseGetRoundTrip(b)),
      "coverage A"                      -> exists((a: A) => laws.demonstratesCoverageA(a)),
      "coverage B"                      -> exists((b: B) => laws.demonstratesCoverageB(b))
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def splitMonoWith(ga: Gen[A])(
    implicit ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    wedge(Arbitrary(ga), ea, ab, eb)

}

object WedgeTests extends Laws {

  def apply[A, B](fab: Wedge[A, B]): WedgeTests[A, B] =
    new WedgeTests[A, B] {
      val laws = new WedgeLaws(fab)
    }

}
