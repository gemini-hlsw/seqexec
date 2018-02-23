// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.laws
package discipline

import cats.Eq
import gem.optics.Wedge
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait WedgeTests[A, B] extends Laws {
  val wedgeLaws: WedgeLaws[A, B]

  def wedge(
    implicit aa: Arbitrary[A], ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("Wedge",
      "normalize A"                     -> forAll((a: A) => wedgeLaws.normalizeA(a)),
      "normalize B"                     -> forAll((b: B) => wedgeLaws.normalizeB(b)),
      "normalized get roundtrip"        -> forAll((a: A) => wedgeLaws.normalizedGetRoundTrip(a)),
      "normalized reverseGet roundtrip" -> forAll((b: B) => wedgeLaws.normalizedReverseGetRoundTrip(b)),
      "coverage A"                      -> exists((a: A) => wedgeLaws.demonstratesCoverageA(a)),
      "coverage B"                      -> exists((b: B) => wedgeLaws.demonstratesCoverageB(b))
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
      val wedgeLaws = new WedgeLaws(fab)
    }

}
