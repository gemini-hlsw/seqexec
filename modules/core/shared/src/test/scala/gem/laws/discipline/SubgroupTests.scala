// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.laws
package discipline

import cats._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait SubgroupTests[A, B] extends Laws {
  val laws: SubgroupLaws[A, B]

  def subgroup(
    implicit aa: Arbitrary[A],
             eq: Eq[B]
  ): SimpleRuleSet = {
    new SimpleRuleSet("subgroup",
      "combine"  -> forAll((a: A, b: A) => laws.combine(a, b)),
      "identity" -> laws.empty,
      "inverse"  -> forAll((a: A) => laws.inverse(a))
    )
  }

}

object SubgroupTests extends Laws {

  def apply[A, B](
    implicit ev: A <:< B,
             ia: Group[A],
             ib: Group[B]
  ): SubgroupTests[A, B] =
    new SubgroupTests[A, B] {
      val laws = new SubgroupLaws[A, B](ev) {
        val A = ia
        val B = ib
      }
    }

}
