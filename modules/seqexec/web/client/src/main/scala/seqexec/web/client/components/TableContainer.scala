// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import react.virtualized._
import react.common.implicits._

/**
  * Container for several types of tables
  */
object TableContainer {

  // Todo use Reusable[A ~=> B]
  final case class Props(hasControls: Boolean, table: Size => VdomElement, onResize: Size => Callback)

  implicit val reuse: Reusability[Props] = Reusability.never

  private val component = ScalaComponent
    .builder[Props]("TableContainer")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.tableContainer.when(p.hasControls),
        SeqexecStyles.tableContainerNoControls.unless(p.hasControls),
        AutoSizer(AutoSizer.props(p.table, onResize = p.onResize))
    ))
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] =
    component(p)
}
