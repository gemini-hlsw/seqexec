// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.implicits._

sealed trait OffsetAxis {
  val configItem: String
}
object OffsetAxis {
  case object AxisP extends OffsetAxis {
    val configItem = "p"
  }
  case object AxisQ extends OffsetAxis {
    val configItem = "q"
  }
  implicit val show: Show[OffsetAxis] = Show.show {
    case AxisP => "p"
    case AxisQ => "q"
  }
}

sealed trait Offset {
  val value: Double
}
object Offset {
  implicit val equal: Eq[Offset] =
    Eq.by {
      case p: TelescopeOffset.P => Left(p)
      case q: TelescopeOffset.Q => Right(q)
    }

  def Zero(axis: OffsetAxis): Offset = axis match {
    case OffsetAxis.AxisP => TelescopeOffset.P.Zero
    case OffsetAxis.AxisQ => TelescopeOffset.Q.Zero
  }
}

// Telescope offsets, roughly based on gem
final case class TelescopeOffset(p: TelescopeOffset.P, q: TelescopeOffset.Q)
object TelescopeOffset {
  /** P component of an angular offset.. */
  final case class P(value: Double) extends Offset
  object P {
    val Zero: P = P(0.0)
    implicit val order: Order[P] = Order.by(_.value)

  }
  /** Q component of an angular offset.. */
  final case class Q(value: Double) extends Offset
  object Q {
    val Zero: Q = Q(0.0)
    implicit val order: Order[Q] = Order.by(_.value)

  }
  implicit val eq: Eq[TelescopeOffset] =
    Eq.by(x => (x.p, x.q))

  implicit val show: Show[TelescopeOffset] = Show.fromToString
}
