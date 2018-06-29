// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package math

import cats.{ Order, Show }
import cats.kernel.CommutativeGroup
import cats.implicits._
import gem.optics.SplitMono
import monocle.{ Iso, Lens }
import monocle.macros.{ GenIso, GenLens }

/** Angular offset with P and Q components. */
final case class Offset(p: Offset.P, q: Offset.Q) {

  /** This offset, with both components reflected around the 0 .. 180° axis. Exact, invertable. */
  def unary_- : Offset =
    Offset(-p, -q)

  /** Componentwise sum of this offset and `o`. Exact. */
  def +(o: Offset): Offset =
    Offset(p + o.p, q + o.q)

  /** This offset pair in radians. */
  def toRadians: (Double, Double) =
    (p.toRadians, q.toRadians)

}

object Offset extends OffsetOptics {

  /** The zero offset. */
  val Zero: Offset =
    Offset(P.Zero, Q.Zero)

  /** Offset forms a commutative group. */
  implicit val CommutativeGroupOffset: CommutativeGroup[Offset] =
    new CommutativeGroup[Offset] {
      val empty: Offset = Zero
      def combine(a: Offset, b: Offset) = a + b
      def inverse(a: Offset) = -a
    }

  implicit val ShowOffset: Show[Offset] =
    Show.fromToString

  /** Offsets are ordered by p, then q. */
  implicit val OrderOffset: Order[Offset] =
    Order.by(o => (o.p, o.q))

  /** P component of an angular offset.. */
  final case class P(toAngle: Angle) {

    /** This P component, reflected around the 0 .. 180° axis. Exact, invertable. */
    def unary_- : P =
      P(-toAngle)

    /** Some of this P component and `p`. Exact. */
    def +(p: P): P =
      P(toAngle + p.toAngle)

    /** This P component in signed radians. */
    def toRadians: Double =
      toAngle.toSignedDoubleRadians

  }
  object P extends POptics {

    /** The zero P component. */
    val Zero: P =
      P(Angle.Angle0)

    /** P forms a commutative group. */
    implicit val CommutativeGroupP: CommutativeGroup[P] =
      new CommutativeGroup[P] {
        val empty: P = Zero
        def combine(a: P, b: P) = a + b
        def inverse(a: P) = -a
      }

    implicit val ShowP: Show[P] =
      Show.fromToString

    /** P components are by signed angle. */
    implicit val OrderP: Order[P] =
      Angle.SignedAngleOrder.contramap(_.toAngle)

  }
  trait POptics {

    /** @Group Optics */
    val angle: Iso[P, Angle] =
      GenIso[P, Angle]

    val signedArcseconds: SplitMono[P, BigDecimal] =
      Angle.signedArcseconds.imapA(P(_), _.toAngle)

  }

  /** Q component of an angular offset.. */
  final case class Q(toAngle: Angle) {

    /** This Q component, reflected around the 0 .. 180° axis. Exact, invertable. */
    def unary_- : Q =
      Q(-toAngle)

    /** Some of this Q component and `p`. Exact. */
    def +(p: Q): Q =
      Q(toAngle + p.toAngle)

    /** This Q component in signed radians. */
    def toRadians: Double =
      toAngle.toSignedDoubleRadians

  }
  object Q extends QOptics {

    /** The zero Q component. */
    val Zero: Q =
      Q(Angle.Angle0)

    /** Q forms a commutative group. */
    implicit val CommutativeGroupQ: CommutativeGroup[Q] =
      new CommutativeGroup[Q] {
        val empty: Q = Zero
        def combine(a: Q, b: Q) = a + b
        def inverse(a: Q) = -a
      }

    implicit val ShowQ: Show[Q] =
      Show.fromToString

    /** Q components are ordered by signed angle. */
    implicit val OrderQ: Order[Q] =
      Angle.SignedAngleOrder.contramap(_.toAngle)

  }
  trait QOptics {

    /** @Group Optics */
    val angle: Iso[Q, Angle] =
      GenIso[Q, Angle]

    val signedArcseconds: SplitMono[Q, BigDecimal] =
      Angle.signedArcseconds.imapA(Q(_), _.toAngle)

  }


}

trait OffsetOptics {

  /** @group Optics */
  val p: Lens[Offset, Offset.P] =
    GenLens[Offset](_.p)

  /** @group Optics */
  val q: Lens[Offset, Offset.Q] =
    GenLens[Offset](_.q)

  /** @group Optics */
  val pAngle: Lens[Offset, Angle] =
    p composeIso Offset.P.angle

  /** @group Optics */
  val qAngle: Lens[Offset, Angle] =
    q composeIso Offset.Q.angle

}