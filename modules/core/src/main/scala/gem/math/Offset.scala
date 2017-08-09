// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package math

import cats.{ Eq, Monoid, Show }
import cats.implicits._

/** Angular offset with P and Q components. */
final case class Offset(p: Offset.P, q: Offset.Q) {

  /** This offset, with both components reflected around the 0 .. 180° axis. Exact, invertable. */
  def unary_- : Offset =
    Offset(-p, -q)

  /** Componentwise sum of this offset and `o`. Exact. */
  def +(o: Offset): Offset =
    Offset(p + o.p, q + o.q)

}

object Offset {

  /** The zero offset. */
  val Zero: Offset =
    Offset(P.Zero, Q.Zero)

  /** Offset forms an Abelian group but Monoid is the best we can do right now. */
  implicit val MonoidOffset: Monoid[Offset] =
    new Monoid[Offset] {
      val empty: Offset = Zero
      def combine(a: Offset, b: Offset) = a + b
    }

  implicit val ShowOffset: Show[Offset] =
    Show.fromToString

  /** Offsets are equal if their components are pairwise equal. */
  implicit val EqualOffset: Eq[Offset] =
    Eq.by(o => (o.p, o.q))


  /** P component of an angular offset.. */
  final case class P(toAngle: Angle) {

    /** This P component, reflected around the 0 .. 180° axis. Exact, invertable. */
    def unary_- : P =
      P(-toAngle)

    /** Some of this P component and `p`. Exact. */
    def +(p: P): P =
      P(toAngle + p.toAngle)

  }
  object P {

    /** The zero P component. */
    val Zero: P =
      P(Angle.Angle0)

    /** P forms an Abelian group but Monoid is the best we can do right now. */
    implicit val MonoidP: Monoid[P] =
      new Monoid[P] {
        val empty: P = Zero
        def combine(a: P, b: P) = a + b
      }

    implicit val ShowP: Show[P] =
      Show.fromToString

    /** P components are equal if their angles are equal. */
    implicit val EqualP: Eq[P] =
      Eq.by(_.toAngle)

  }

  /** Q component of an angular offset.. */
  final case class Q(toAngle: Angle) {

    /** This Q component, reflected around the 0 .. 180° axis. Exact, invertable. */
    def unary_- : Q =
      Q(-toAngle)

    /** Some of this Q component and `p`. Exact. */
    def +(p: Q): Q =
      Q(toAngle + p.toAngle)

  }
  object Q {

    /** The zero Q component. */
    val Zero: Q =
      Q(Angle.Angle0)

    /** Q forms an Abelian group but Monoid is the best we can do right now. */
    implicit val MonoidQ: Monoid[Q] =
      new Monoid[Q] {
        val empty: Q = Zero
        def combine(a: Q, b: Q) = a + b
      }

    implicit val ShowQ: Show[Q] =
      Show.fromToString

    /** Q components are equal if their angles are equal. */
    implicit val EqualQ: Eq[Q] =
      Eq.by(_.toAngle)

  }

}
