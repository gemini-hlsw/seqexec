// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import seqexec.web.client.components.SeqexecStyles

case class BiasStatus(fileId: String) extends ReactProps {
  @inline def render: VdomElement = BiasStatus.component(this)
}

object BiasStatus {
  type Props = BiasStatus

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("ObservationProgressDisplay")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.specialStateLabel,
        p.fileId
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build
}
