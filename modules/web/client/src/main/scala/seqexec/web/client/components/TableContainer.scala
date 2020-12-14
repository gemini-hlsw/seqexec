// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.virtualized._

final case class TableContainer(
  hasControls: Boolean,
  table:       Size => VdomElement,
  onResize:    Size => Callback
) extends ReactProps[TableContainer](TableContainer.component)

/**
 * Container for several types of tables
 */
object TableContainer {
  type Props = TableContainer

  implicit val reuse: Reusability[Props] = Reusability.never

  private val component = ScalaComponent
    .builder[Props]("TableContainer")
    .stateless
    .render_P(p =>
      AutoSizer(AutoSizer.props(p.table, onResize = p.onResize))
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] =
    component(p)
}
