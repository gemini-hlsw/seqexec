// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import _root_.boopickle.Default.Pickler
import _root_.boopickle.Default.Pickle
import _root_.boopickle.Default.Unpickle
import cats.laws._
import cats.laws.discipline._
import cats.kernel.Eq
import org.scalacheck.{Arbitrary, Prop, Shrink}
import org.typelevel.discipline.Laws

package boopickle {

  trait PicklerLaws[A] {
    implicit def pickler: Pickler[A]

    def picklerRoundTrip(a: A): IsEq[A] =
      Unpickle[A].fromBytes(Pickle.intoBytes(a)) <-> a

  }

  object PicklerLaws {

    def apply[A](implicit picklerA: Pickler[A]): PicklerLaws[A] =
      new PicklerLaws[A] {
        override def pickler: Pickler[A] = picklerA
      }
  }

  trait PicklerTests[A] extends Laws {
    def laws: PicklerLaws[A]

    def pickler(implicit
                arbitraryA: Arbitrary[A],
                shrinkA: Shrink[A],
                eqA: Eq[A]): RuleSet = new DefaultRuleSet(
      name = "codec",
      parent = None,
      "roundTrip" -> Prop.forAll { (a: A) =>
        laws.picklerRoundTrip(a)
      }
    )
  }

  object PicklerTests {

    def apply[A: Pickler]: PicklerTests[A] = new PicklerTests[A] {
      val laws: PicklerLaws[A] = PicklerLaws[A]
    }
  }
}
