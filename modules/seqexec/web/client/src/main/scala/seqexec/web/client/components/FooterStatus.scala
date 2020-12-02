// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.syntax.all._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.semanticui.elements.header.Header
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.reusability._

final case class FooterStatus(status: ClientStatus) extends ReactProps[FooterStatus](FooterStatus.component)

/**
  * Chooses to display either the guide config or a connection status info
  */
object FooterStatus {

  type Props = FooterStatus

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  private val wsConnect                       = SeqexecCircuit.connect(_.ws)
  private val gcConnect                       = SeqexecCircuit.connect(_.guideConfig)

  private val component = ScalaComponent
    .builder[Props]("FooterStatus")
    .stateless
    .render_P(p =>
      React.Fragment(
        Header(sub = true, clazz = SeqexecStyles.item |+| SeqexecStyles.notInMobile)(
          wsConnect(x => ConnectionState(x())).unless(p.status.isConnected)
        ),
        Header(sub = true, clazz = SeqexecStyles.item |+| SeqexecStyles.notInMobile)(
          gcConnect(x => GuideConfigStatus(x())).when(p.status.isConnected)
        ),
        ControlMenu(p.status)
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

}
