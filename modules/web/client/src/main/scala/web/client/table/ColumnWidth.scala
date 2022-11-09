// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.Eq
import cats.syntax.all._
import japgolly.scalajs.react.Reusability
import mouse.boolean._

sealed trait ColumnWidth
sealed abstract class FixedColumnWidth(val width: Double) extends ColumnWidth {
  assert(width >= 0)

  override def toString: String = s"Fixed(${width}px)"
}
sealed abstract class VariableColumnWidth(val percentage: Double, val minWidth: Double)
    extends ColumnWidth {
  assert(percentage > 0 && percentage <= 1)
  assert(minWidth >= 0)
  override def toString: String =
    s"Variable($percentage%, ${minWidth}px)"
}

object ColumnWidth {
  implicit val eq: Eq[ColumnWidth] =
    Eq[Either[FixedColumnWidth, VariableColumnWidth]].contramap {
      case x: FixedColumnWidth    => x.asLeft
      case x: VariableColumnWidth => x.asRight
    }

  implicit val columnWidthReuse: Reusability[ColumnWidth] = Reusability {
    case (a: FixedColumnWidth, b: FixedColumnWidth)       =>
      !FixedColumnWidth.fixedColWidthReuse.testNot(a, b)
    case (a: VariableColumnWidth, b: VariableColumnWidth) =>
      !VariableColumnWidth.variableColWidthReuse.testNot(a, b)
    case _                                                => false
  }
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

  private implicit val doubleReuse: Reusability[Double]          =
    Reusability.double(0.1)
  implicit val fixedColWidthReuse: Reusability[FixedColumnWidth] =
    Reusability.by(_.width)
}

object VariableColumnWidth {
  implicit val eqPcw: Eq[VariableColumnWidth] =
    Eq.by(x => (x.percentage, x.minWidth))

  private final case class VariableColumnWidthI(
    override val percentage: Double,
    override val minWidth:   Double
  ) extends VariableColumnWidth(percentage, minWidth)

  private[table] def apply(
    percentage: Double,
    minWidth:   Double
  ): VariableColumnWidth =
    VariableColumnWidthI(percentage, minWidth)

  def fromDouble(
    percentage: Double,
    minWidth:   Double
  ): Option[VariableColumnWidth] =
    (percentage > 0 && percentage <= 1 && minWidth > 0)
      .option(VariableColumnWidth(percentage, minWidth))

  def unsafeFromDouble(percentage: Double, minWidth: Double): VariableColumnWidth =
    fromDouble(percentage, minWidth).getOrElse(
      sys.error(s"Incorrect percentage/minWidth value $percentage/$minWidth")
    )

  def unapply(
    pc: VariableColumnWidth
  ): Option[(Double, Double)] =
    Some((pc.percentage, pc.minWidth))

  val Full: VariableColumnWidth = VariableColumnWidth(1, 1)
  val Half: VariableColumnWidth = VariableColumnWidth(0.5, 1)

  // Deltas are very small when resizing a col
  private implicit val doubleReuse: Reusability[Double]                =
    Reusability.double(0.0001)
  implicit val variableColWidthReuse: Reusability[VariableColumnWidth] =
    Reusability.by(x => (x.percentage, x.minWidth))
}
