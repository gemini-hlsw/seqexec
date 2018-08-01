// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.Callback
import org.scalajs.dom.MouseEvent
import scala.scalajs.js
import monocle.Lens
import mouse.boolean._
import react.virtualized._
import react.draggable._
import web.client.utils._

package table {

  sealed trait UserModified extends Product with Serializable
  case object IsModified  extends UserModified
  case object NotModified extends UserModified

  object UserModified {
    implicit val eq: Eq[UserModified] = Eq.fromUniversalEquals

    def fromBool(b: Boolean): UserModified = if (b) IsModified else NotModified
  }

  sealed trait ColumnWidth extends Product with Serializable
  final case class FixedColumnWidth private[table] (width: Int) extends ColumnWidth {
    assert(width >= 0)
  }
  final case class PercentageColumnWidth private[table] (percentage: Double) extends ColumnWidth {
    assert(percentage >= 0 && percentage <= 1)
  }

  object ColumnWidth {
    implicit val eq: Eq[ColumnWidth] = Eq[Either[FixedColumnWidth, PercentageColumnWidth]].contramap {
      case x: FixedColumnWidth      => x.asLeft
      case x: PercentageColumnWidth => x.asRight
    }
  }

  object FixedColumnWidth {
    implicit val eqFcw: Eq[FixedColumnWidth] = Eq.by(_.width)

    def fromInt(width: Int): Option[FixedColumnWidth] =
      (width >= 0) option FixedColumnWidth(width)
  }

  object PercentageColumnWidth {
    implicit val eqPcw: Eq[PercentageColumnWidth] = Eq.by(_.percentage)
    private[table] def apply(d: Double) = new PercentageColumnWidth(d)

    def fromDouble(percentage: Double): Option[PercentageColumnWidth] =
      (percentage >= 0 && percentage <= 1) option PercentageColumnWidth(percentage)

    def unsafeFromDouble(percentage: Double): PercentageColumnWidth =
      fromDouble(percentage).getOrElse(sys.error(s"Incorrect percentage value $percentage"))

    val Full: PercentageColumnWidth = PercentageColumnWidth(1)
    val Zero: PercentageColumnWidth = PercentageColumnWidth(0)
    val Half: PercentageColumnWidth = PercentageColumnWidth(0.5)
  }

  final case class ColumnRenderArgs[A](meta: ColumnMeta[A], index: Int, width: JsNumber, resizable: Boolean)

  /**
   * State of a table
   */
  final case class TableState[A: Eq](userModified: UserModified, scrollPosition: JsNumber, columns: NonEmptyList[ColumnMeta[A]]) {

    // Changes the relative widths when a column is being dragged
    def applyOffset(column: A, delta: Double): TableState[A] = {
      val indexOf = columns.toList.indexWhere(_.column === column)
      // Shift the selected column and the next one
      val result = columns.toList.zipWithIndex.map {
        case (c @ ColumnMeta(_, _, _, true, PercentageColumnWidth(x)), idx) if idx === indexOf     =>
          val nv = x + delta
          (nv >= 0 && nv <= 1).fold(c.copy(width = PercentageColumnWidth(nv)), c)
        case (c @ ColumnMeta(_, _, _, true, PercentageColumnWidth(x)), idx) if idx === indexOf + 1 =>
          val nv = x - delta
          (nv >= 0 && nv <= 1).fold(c.copy(width = PercentageColumnWidth(nv)), c)
        case (c, _)                                                                                => c
      }
      copy(userModified = IsModified, columns = NonEmptyList.fromListUnsafe(result))
    }

    // Return the width of a column from the actual column width
    def widthOf(column: A, s: Size): Double =
      columns
        .filter(c => c.column === column && c.visible)
        .map(_.width)
        .map {
          case FixedColumnWidth(w)      => w.toDouble
          case PercentageColumnWidth(f) => f * s.width
        }
        .headOption
        .getOrElse(0.0)

    def columnBuilder(s: Size, b: ColumnRenderArgs[A] => Table.ColumnArg): List[Table.ColumnArg] = {
      val disposableWidth = math.max(0, s.width.toDouble - columns.collect {
        case ColumnMeta(_, _, _, true, FixedColumnWidth(x)) => x
      }.sum)
      normalizeColumnsPercentages.columns.toList.zipWithIndex.map {
        case (m @ ColumnMeta(_, _, _, true, FixedColumnWidth(w)), i) =>
          b.apply(ColumnRenderArgs(m, i, w, i < (columns.length - 1)))
        case (m @ ColumnMeta(_, _, _, true, PercentageColumnWidth(p)), i) =>
          b.apply(ColumnRenderArgs(m, i, p * disposableWidth, i < (columns.length - 1)))
      }
    }

    // normalize the percentages
    def normalizeColumnsPercentages: TableState[A] = {
      val percentagesSum = columns.collect {
        case ColumnMeta(_, _, _, true, PercentageColumnWidth(x)) => x
      }.sum
      TableState.columns[A].modify(_.map {
        case c @ ColumnMeta(_, _, _, true, PercentageColumnWidth(x)) =>
          c.copy(width = PercentageColumnWidth(x / percentagesSum))
        case c =>
          c
      })(this)
    }

    // Tell the model to resize a column
    def resizeRow(column: A, s: Size, cb: TableState[A] => Callback): (String, JsNumber) => Callback =
      (_, dx) => {
        val percentDelta = dx.toDouble / s.width.toDouble
        val st           = applyOffset(column, percentDelta)
        cb(st)
      }
  }

  object TableState {
    implicit def eqTs[A: Eq]: Eq[TableState[A]] =
      Eq.by(x => (x.userModified, x.scrollPosition.toDouble, x.columns))

    def userModified[A: Eq]: Lens[TableState[A], UserModified] =
      Lens[TableState[A], UserModified](_.userModified)(n => a => a.copy(userModified = n))

    def columns[A: Eq]: Lens[TableState[A], NonEmptyList[ColumnMeta[A]]] =
      Lens[TableState[A], NonEmptyList[ColumnMeta[A]]](_.columns)(n => a => a.copy(columns = n))

    def scrollPosition[A: Eq]: Lens[TableState[A], JsNumber] =
      Lens[TableState[A], JsNumber](_.scrollPosition)(n => a => a.copy(scrollPosition = n))
  }

  /**
   * Metadata for a column
   */
  final case class ColumnMeta[A](column: A, name: String, label: String, visible: Boolean, width: ColumnWidth)

  object ColumnMeta {
    implicit def eqCm[A: Eq]: Eq[ColumnMeta[A]] = Eq.by(x => (x.column, x.name, x.label, x.visible, x.width))
  }

}

package object table {
  // Renderer for a resizable column
  def resizableHeaderRenderer(
      rs: (String, JsNumber) => Callback): HeaderRenderer[js.Object] =
    (_, dataKey: String, _, label: VdomNode, _, _) =>
      ReactFragment.withKey(dataKey)(
        <.div(
          ^.cls := "ReactVirtualized__Table__headerTruncatedText",
          label
        ),
        Draggable(
          Draggable.props(
            axis = Axis.X,
            defaultClassName = "DragHandle",
            defaultClassNameDragging = "DragHandleActive",
            onDrag = (ev: MouseEvent, d: DraggableData) => rs(dataKey, d.deltaX),
            position = ControlPosition(0)
          ),
          <.span(^.cls := "DragHandleIcon", "⋮")
        )
    )
}
