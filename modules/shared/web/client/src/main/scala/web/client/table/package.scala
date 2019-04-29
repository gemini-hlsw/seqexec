// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import cats.Monoid
import cats.data.NonEmptyList
import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.React
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra._
import org.scalajs.dom.MouseEvent
import scala.scalajs.js
import js.JSConverters._
import react.virtualized._
import react.virtualized.raw
import react.common._
import react.common.syntax._
import react.sortable._
import react.draggable._
import web.client.utils._

package object table {
  val DragHandleWidth: Int = 12

  private implicit val doubleReuse: Reusability[Double] =
    Reusability.double(0.01)

  implicit val sizeReuse: Reusability[Size] =
    Reusability.by(x => (x.width, x.height))

  implicit def nelR[A: Reusability]: Reusability[NonEmptyList[A]] =
    Reusability.by(_.toList)

  implicit def tsR[A: Reusability]: Reusability[TableState[A]] =
    Reusability.by(x => (x.userModified, x.scrollPosition, x.columns))

  // Renderer for a resizable column
  def resizableHeaderRenderer(
    rs: => (String, JsNumber) => Callback): HeaderRenderer[js.Object] =
    (_, dataKey: String, _, label: VdomNode, _, _) =>
      React.Fragment.withKey(dataKey)(
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

  implicit val styleMonoid: Monoid[Style] = new Monoid[Style] {
    override val empty: Style = Style(Map.empty)
    override def combine(a: Style, b: Style): Style =
      Style(a.styles ++ b.styles)
  }

  private implicit class ClickCallbackOps(val cb: OnRowClick) extends AnyVal {
    def toJsCallback: raw.RawOnRowEvent =
      (i: raw.RawIndexParameter) => cb(i.index).runNow()
  }

  def sortableRowRenderer[C <: js.Object](
    extraStyle: (Int, Style) => Style): RowRenderer[C] =
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
      val mergedStyle  = (style |+| extraStyle(index, style)).toJsObject
      sortableItem(
        SortableElement.Props(index = index, key = key, style = mergedStyle))(
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
          mergedStyle
        )))

    }
}
