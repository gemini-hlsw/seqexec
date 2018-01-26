// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.laws
package discipline

import cats.Eq
import gem.util.Section
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait SectionTests[A, B] extends Laws {
  val laws: SectionLaws[A, B]

  def section(
    implicit aa: Arbitrary[A], ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("Section",
      "normalize"                -> forAll((a: A) => laws.normalize(a)),
      "normalized get roundtrip" -> forAll((a: A) => laws.normalizedGetRoundTrip(a)),
      "reverseGet roundtrip"     -> forAll((b: B) => laws.reverseGetRoundTrip(b)),
      "coverage"                 -> exists((a: A) => laws.demonstratesNormalization(a))
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def sectionWith(ga: Gen[A])(
    implicit ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    section(Arbitrary(ga), ea, ab, eb)

}

object SectionTests extends Laws {

  def apply[A, B](fab: Section[A, B]): SectionTests[A, B] =
    new SectionTests[A, B] {
      val laws = new SectionLaws(fab)
    }

}
