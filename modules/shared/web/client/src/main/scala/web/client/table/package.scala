// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import cats.data.NonEmptyList
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.Callback
import org.scalajs.dom.MouseEvent
import react.virtualized._
import react.draggable._
import scala.scalajs.js

package table {
  final case class TableState[A](userModified: Boolean, columns: NonEmptyList[A])
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
