// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.Eq
import cats.implicits._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.CatsReact._
import mouse.boolean._

sealed trait ColumnWidth
sealed abstract class FixedColumnWidth(val width: Double)
    extends ColumnWidth {
  assert(width >= 0)

  override def toString: String = s"Fixed(${width}px)"
}
sealed abstract class PercentageColumnWidth(val percentage: Double,
                                            val minWidth:   Double)
    extends ColumnWidth {
  assert(percentage >= 0 && percentage <= 1)
  assert(minWidth >= 0)
  override def toString: String = s"Percentage(${percentage}%, ${minWidth}px)"
}

object ColumnWidth {
  implicit val eq: Eq[ColumnWidth] =
    Eq[Either[FixedColumnWidth, PercentageColumnWidth]].contramap {
      case x: FixedColumnWidth      => x.asLeft
      case x: PercentageColumnWidth => x.asRight
    }

  implicit val reuse: Reusability[ColumnWidth] = Reusability.byEq
}

object FixedColumnWidth {
  implicit val eqFcw: Eq[FixedColumnWidth] = Eq.by(_.width)
  private[table] def apply(p: Double) = new FixedColumnWidth(p) {}

  def fromDouble(width: Double): Option[FixedColumnWidth] =
    (width >= 0).option(FixedColumnWidth(width))

  def unsafeFromDouble(width: Double): FixedColumnWidth =
    fromDouble(width).getOrElse(sys.error(s"Incorrect width value $width"))

  def unapply(fc: FixedColumnWidth): Option[Double] =
    Some(fc.width)
}

object PercentageColumnWidth {
  implicit val eqPcw: Eq[PercentageColumnWidth] =
    Eq.by(x => (x.percentage, x.minWidth))
  private[table] def apply(p: Double, mp: Double) =
    new PercentageColumnWidth(p, mp) {}

  def fromDouble(percentage: Double,
                 minWidth:   Double): Option[PercentageColumnWidth] =
    (percentage >= 0 && percentage <= 1 && minWidth >= 0)
      .option(PercentageColumnWidth(percentage, minWidth))

  def unsafeFromDouble(percentage: Double,
                       minWidth:   Double): PercentageColumnWidth =
    fromDouble(percentage, minWidth).getOrElse(
      sys.error(s"Incorrect percentage/minWidth value $percentage/$minWidth"))

  def unapply(pc: PercentageColumnWidth): Option[(Double, Double)] =
    Some((pc.percentage, pc.minWidth))

  val Full: PercentageColumnWidth = PercentageColumnWidth(1, 0)
  val Zero: PercentageColumnWidth = PercentageColumnWidth(0, 0)
  val Half: PercentageColumnWidth = PercentageColumnWidth(0.5, 0)
}
