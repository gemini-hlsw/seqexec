// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.Index
import gem.syntax.prism._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._

trait ArbIndex {

  val genIndex: Gen[Index] =
    choose[Short](0, Short.MaxValue).map(Index.fromShort.unsafeGet)

  implicit val arbIndex: Arbitrary[Index] =
    Arbitrary(genIndex)

  implicit val cogIndex: Cogen[Index] =
    Cogen[Short].contramap(_.toShort)

  private val perturbations: List[String => Gen[String]] =
    List(
      s => arbitrary[String], // swap for a random string
      s => Gen.const("0" + s) // prepend a zero
    )

  // Strings that are often parsable as an Index.
  val strings: Gen[String] =
    arbitrary[Short].map(_.toString).flatMapOneOf(Gen.const, perturbations: _*)

}

object ArbIndex extends ArbIndex