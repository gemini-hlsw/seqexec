// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import japgolly.scalajs.react.CtorType
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.vdom.html_<^._
import react.virtualized._

object SortableRow {
  final case class Props(p: react.virtualized.raw.RawRowRendererParameter)

  val component: Component[Props, Unit, Unit, CtorType.Props] = ScalaComponent
    .builder[Props]("SortableRow")
    .render_P { p =>
      defaultRowRenderer(p.p)
    }
    .build

}
