// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.CatsReact._
import org.scalajs.dom.MouseEvent
import scala.scalajs.js
import js.JSConverters._
import monocle.Lens
import mouse.boolean._
import react.virtualized._
import react.virtualized.raw
import react.sortable._
import react.draggable._
import web.client.utils._

package table {

  sealed trait UserModified extends Product with Serializable
  case object IsModified extends UserModified
  case object NotModified extends UserModified

  object UserModified {
    implicit val eq: Eq[UserModified] = Eq.fromUniversalEquals

    implicit val reuse: Reusability[UserModified] = Reusability.byRef

    def fromBool(b: Boolean): UserModified = if (b) IsModified else NotModified
  }

  sealed trait ColumnWidth // extends Product with Serializable
  sealed abstract class FixedColumnWidth(val width: Double)
      extends ColumnWidth {
    assert(width >= 0)
  }
  sealed abstract class PercentageColumnWidth(val percentage: Double,
                                              val minWidth:   Double)
      extends ColumnWidth {
    assert(percentage >= 0 && percentage <= 1)
    assert(minWidth >= 0)
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

  final case class ColumnRenderArgs[A](meta:      ColumnMeta[A],
                                       index:     Int,
                                       width:     JsNumber,
                                       resizable: Boolean)

  /**
    * State of a table
    */
  final case class TableState[A: Eq](userModified:   UserModified,
                                     scrollPosition: JsNumber,
                                     columns:        NonEmptyList[ColumnMeta[A]]) {

    // Changes the relative widths when a column is being dragged
    def applyOffset(column: A, delta: Double, s: Size): TableState[A] = {
      val indexOf = columns.toList.indexWhere(_.column === column)
      // Shift the selected column and the next one
      val result = columns.toList.zipWithIndex.map {
        case (c @ ColumnMeta(_, _, _, true, PercentageColumnWidth(x, min)), idx) if idx === indexOf     =>
          val nv = x + delta
          if (nv >= 0 && nv <= 1) {
            c.copy(width = PercentageColumnWidth(nv, min))
          } else {
            c
          }
        case (c @ ColumnMeta(_, _, _, true, PercentageColumnWidth(x, min)), idx) if idx === indexOf + 1 =>
          val nv = scala.math.max(min / s.width, x - delta)
          if (nv >= 0 && nv <= 1) {
            c.copy(width = PercentageColumnWidth(nv, min))
          } else {
            c
          }
        case (c, _)                                                                                => c
      }
      copy(userModified = IsModified,
           columns      = NonEmptyList.fromListUnsafe(result))
    }

    // Return the width of a column from the actual column width
    def widthOf(column: A, s: Size): Double =
      columns
        .filter(c => c.column === column && c.visible)
        .map(_.width)
        .map {
          case FixedColumnWidth(w)         => w.toDouble
          case PercentageColumnWidth(f, m) => scala.math.max(m, f * s.width)
        }
        .foldMap(identity)

    // Table can call this to build the columns
    def columnBuilder(
      s:  Size,
      cb: ColumnRenderArgs[A] => Table.ColumnArg): List[Table.ColumnArg] = {
      val disposableWidth = math.max(0, s.width.toDouble - columns.collect {
        case ColumnMeta(_, _, _, true, FixedColumnWidth(x)) => x
      }.sum)
      normalizeColumnsPercentages(s).columns.toList.zipWithIndex.map {
        case (m @ ColumnMeta(_, _, _, true, FixedColumnWidth(w)), i) =>
          cb.apply(ColumnRenderArgs(m, i, w, false))
        case (m @ ColumnMeta(_, _, _, true, PercentageColumnWidth(p, mw)), i) =>
          cb.apply(ColumnRenderArgs(m,
                                   i,
                                   scala.math.max(mw, p * disposableWidth),
                                   i < (columns.length - 1)))
      }
    }

    // normalize the percentages
    def normalizeColumnsPercentages(s: Size): TableState[A] = {
      val percentagesSum = columns.collect {
        case ColumnMeta(_, _, _, true, PercentageColumnWidth(x, m)) => scala.math.max(x, m / s.width)
      }.sum
      TableState
        .columns[A]
        .modify(_.map {
          case c @ ColumnMeta(_, _, _, true, PercentageColumnWidth(x, min)) =>
            c.copy(width = PercentageColumnWidth(x / percentagesSum, min))
          case c                                                            =>
            c
        })(this)
    }

    // Tell the model to resize a column
    def resizeRow(
      column: A,
      s:      Size,
      cb:     TableState[A] => Callback): (String, JsNumber) => Callback =
      (_, dx) => {
        val percentDelta = dx.toDouble / s.width.toDouble
        val st           = applyOffset(column, percentDelta, s)
        cb(st)
      }
  }

  object TableState {
    implicit def eqTs[A: Eq]: Eq[TableState[A]] =
      Eq.by(x => (x.userModified, x.scrollPosition.toDouble, x.columns))

    def userModified[A: Eq]: Lens[TableState[A], UserModified] =
      Lens[TableState[A], UserModified](_.userModified)(n =>
        a => a.copy(userModified = n))

    def columns[A: Eq]: Lens[TableState[A], NonEmptyList[ColumnMeta[A]]] =
      Lens[TableState[A], NonEmptyList[ColumnMeta[A]]](_.columns)(n =>
        a => a.copy(columns = n))

    def scrollPosition[A: Eq]: Lens[TableState[A], JsNumber] =
      Lens[TableState[A], JsNumber](_.scrollPosition)(n =>
        a => a.copy(scrollPosition = n))
  }

  /**
    * Metadata for a column
    */
  final case class ColumnMeta[A](column:  A,
                                 name:    String,
                                 label:   String,
                                 visible: Boolean,
                                 width:   ColumnWidth)

  object ColumnMeta {
    implicit def eqCm[A: Eq]: Eq[ColumnMeta[A]] =
      Eq.by(x => (x.column, x.name, x.label, x.visible, x.width))

    implicit def reuse[A: Reusability]: Reusability[ColumnMeta[A]] =
      Reusability.derive[ColumnMeta[A]]
  }

}

package object table {
  val DragHandleWidth: Int = 12

  implicit def nelR[A: Reusability]: Reusability[NonEmptyList[A]] =
    Reusability.by(_.toList)

  implicit def tsR[A: Reusability]: Reusability[TableState[A]] =
    Reusability.derive[TableState[A]]

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
            axis                     = Axis.X,
            defaultClassName         = "DragHandle",
            defaultClassNameDragging = "DragHandleActive",
            onDrag                   = (ev: MouseEvent, d: DraggableData) => rs(dataKey, d.deltaX),
            position                 = ControlPosition(0)
          ),
          <.span(^.cls := "DragHandleIcon", "⋮")
        )
    )

  def sortableRowRenderer[C <: js.Object]: RowRenderer[C] =
    (className:        String,
     columns:          Array[VdomNode],
     index:            Int,
     isScrolling:      Boolean,
     key:              String,
     rowData:          C,
     onRowClick:       Option[OnRowClick],
     onRowDoubleClick: Option[OnRowClick],
     onRowMouseOut:    Option[OnRowClick],
     onRowMouseOver:   Option[OnRowClick],
     onRowRightClick:  Option[OnRowClick],
     style:            Style) => {
      val sortableItem = SortableElement.wrap(SortableRow.component)
      sortableItem(SortableElement.Props(index = index))(
        SortableRow.Props(raw.RawRowRendererParameter(
          className,
          columns.map(_.rawNode).toJSArray,
          index,
          isScrolling,
          key,
          rowData,
          onRowClick.map(_.toJsCallback).orUndefined,
          onRowDoubleClick.map(_.toJsCallback).orUndefined,
          onRowMouseOut.map(_.toJsCallback).orUndefined,
          onRowMouseOver.map(_.toJsCallback).orUndefined,
          onRowRightClick.map(_.toJsCallback).orUndefined,
          Style.toJsObject(style)
        )))

    }
}
