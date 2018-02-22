// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.laws
package discipline

import cats.Eq
import cats.instances.option._
import gem.optics.Format
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait FormatTests[A, B] extends Laws {
  val laws: FormatLaws[A, B]

  def format(
    implicit aa: Arbitrary[A], ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("format",
      "normalize"        -> forAll((a: A) => laws.normalize(a)),
      "parse roundtrip"  -> forAll((a: A) => laws.parseRoundTrip(a)),
      "format roundtrip" -> forAll((b: B) => laws.formatRoundTrip(b)),
      "coverage"         -> exists((a: A) => laws.demonstratesNormalization(a))
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def formatWith(ga: Gen[A])(
    implicit ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    format(Arbitrary(ga), ea, ab, eb)

}

object FormatTests extends Laws {

  def apply[A, B](fab: Format[A, B]): FormatTests[A, B] =
    new FormatTests[A, B] {
      val laws = new FormatLaws(fab)
    }

}
