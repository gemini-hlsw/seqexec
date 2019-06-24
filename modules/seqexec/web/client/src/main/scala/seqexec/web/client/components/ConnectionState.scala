// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import diode.react.ModelProxy
import diode.react.ReactPot._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import react.common.implicits._
import seqexec.web.client.model.WebSocketConnection
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.reusability._

/**
  * Alert message when the connection disappears
  */
object ConnectionState {

  final case class Props(u: WebSocketConnection)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def formatTime(delay: Int): String =
    if (delay < 1000) {
      f"${delay / 1000.0}%.1f"
    } else {
      f"${delay / 1000}%d"
    }

  private val component = ScalaComponent
    .builder[Props]("ConnectionState")
    .stateless
    .render_P(
      p =>
        <.div(
          ^.cls := "ui header item sub",
          p.u.ws.renderPending(
            _ =>
              <.div(
                IconAttention.copyIcon(color = Option("red")),
                <.span(
                  SeqexecStyles.errorText,
                  s"Connection lost, retrying in ${formatTime(p.u.nextAttempt)} [s] ..."
                )
              )
          )
        )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(u: ModelProxy[WebSocketConnection]): Unmounted[Props, Unit, Unit] =
    component(Props(u()))
}
