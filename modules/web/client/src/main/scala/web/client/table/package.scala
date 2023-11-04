// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import scala.math.max
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

import cats.Foldable
import cats.data.NonEmptyList
import cats.syntax.all._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.React
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.facade.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.MouseEvent
import react.common._
import react.common.implicits._
import react.draggable._
import react.sortable.SortableElement
import react.virtualized._
import react.virtualized.raw
import web.client.utils._

package object table {
  val DragHandleWidth: Int = 12

  private[table] implicit val doubleReuse: Reusability[Double] =
    Reusability.double(0.01)

  implicit val sizeReuse: Reusability[Size] =
    Reusability.by(x => (x.width, x.height))

  implicit def nelR[A: Reusability]: Reusability[NonEmptyList[A]] =
    Reusability.by(_.toList)

  implicit def tsR[A: Reusability]: Reusability[TableState[A]] =
    Reusability.by(x => (x.userModified, x.scrollPosition, x.columns))

  // Renderer for a resizable column
  def resizableHeaderRenderer(rs: => (String, JsNumber) => Callback): HeaderRenderer[js.Object] =
    (_, dataKey: String, _, label: VdomNode, _, _) =>
      React.Fragment.withKey(dataKey)(
        <.div(
          ^.cls := "ReactVirtualized__Table__headerTruncatedText",
          label
        ),
        Draggable(
          Draggable.props(
            axis = Axis.X,
            defaultClassName = "DragHandle",
            defaultClassNameDragging = "DragHandleActive",
            onDrag = (_: MouseEvent, d: DraggableData) => rs(dataKey, d.deltaX),
            position = ControlPosition(0)
          ),
          <.span(^.cls := "DragHandleIcon", "⋮")
        )
      )

  private implicit class ClickCallbackOps(val cb: OnRowClick) extends AnyVal {
    def toJsCallback: raw.RawOnRowEvent =
      (i: raw.RawIndexParameter) => cb(i.index).runNow()
  }

  def sortableRowRenderer[C <: js.Object](extraStyle: (Int, Style) => Style): RowRenderer[C] =
    (
      className:        String,
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
      style:            Style
    ) => {
      val sortableItem = SortableElement.wrap(SortableRow.component)
      val mergedStyle  = (style |+| extraStyle(index, style)).toJsObject

      sortableItem(SortableElement.Props(index = index, key = key, style = mergedStyle))(
        SortableRow.Props(
          raw.RawRowRendererParameter(
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
            mergedStyle
          )
        )
      )

    }

  def colWidths[A, B, G[_]: Foldable](
    items: G[A],
    cols:  NonEmptyList[B],
    get:   Map[B, A => String],
    minW:  Map[B, Double],
    adj:   Map[B, Double]
  ): B => Option[Double] =
    colWidthsO[A, B, G](items, cols, get.view.mapValues(f => (a: A) => f(a).some).toMap, minW, adj)

  /**
   * This methods traverses a whole set of data to find the widest value per column, trying to
   * traverse it in one pass
   */
  def colWidthsO[A, B, G[_]: Foldable](
    items: G[A],
    cols:  NonEmptyList[B],
    get:   Map[B, A => Option[String]],
    minW:  Map[B, Double],
    adj:   Map[B, Double]
  ): B => Option[Double] =
    // Find the longest string per column
    items
      .foldLeft(Map.empty[B, Option[(Int, String)]]) { (cw, a) =>
        val m: NonEmptyList[(B, Option[(Int, String)])] = cols.map { b =>
          b -> get.get(b).flatMap { fb =>
            fb(a).flatMap { v =>
              cw.get(b)
                .map {
                  case b @ Some((l, _)) =>
                    val vl = v.length
                    if (vl > l) (vl, v).some else b
                  case _                => none
                }
                .getOrElse((v.length, v).some)
            }
          }
        }
        m.toList.toMap
      }
      .collect { case (b, Some((_, t))) =>
        b -> {
          // We calculate the actual pixel width at the end
          val v = tableTextWidth(t) + adj.get(b).orEmpty
          minW.get(b).map(max(_, v)).getOrElse(v)
        }
      }
      .get

}
