// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import react.common.implicits._
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.reusability._

/**
  * Chooses to display either the guide config or a connection status info
  */
object FooterStatus {

  final case class Props(status: ClientStatus)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  private val wsConnect                       = SeqexecCircuit.connect(_.ws)
  private val gcConnect                       = SeqexecCircuit.connect(_.guideConfig)

  private val component = ScalaComponent
    .builder[Props]("FooterStatus")
    .stateless
    .render_P(
      p =>
        React.Fragment(
          <.div(SeqexecStyles.notInMobile,
                ^.cls := s"ui header item sub",
                wsConnect(ConnectionState.apply).unless(p.status.isConnected)),
          <.div(SeqexecStyles.notInMobile,
                ^.cls := s"ui header item sub",
                gcConnect(GuideConfigStatus.apply).when(p.status.isConnected)),
          ControlMenu(p.status)
        )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(u: ModelProxy[ClientStatus]): Unmounted[Props, Unit, Unit] =
    component(Props(u()))
}
