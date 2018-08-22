// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import diode.react.ModelProxy
import diode.react.ReactPot._
import gem.enum.Site
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import seqexec.web.client.actions.SelectEmptyPreview
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.WebSocketConnection
import seqexec.web.client.model.Pages._
import seqexec.web.client.OcsBuildInfo
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.elements.menu.HeaderItem
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Component for the bar at the top of the page
  */
object Footer {
  final case class Props(router: RouterCtl[SeqexecPages], site: Site)

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.site)

  private val userConnect = SeqexecCircuit.connect(SeqexecCircuit.statusReader)
  private val wsConnect = SeqexecCircuit.connect(_.ws)

  private def goHome(p: Props)(e: ReactEvent): Callback =
    e.preventDefaultCB *>
    p.router.dispatchAndSetUrlCB(SelectEmptyPreview)

  private val component = ScalaComponent.builder[Props]("SeqexecAppBar")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui footer inverted menu",
        <.a(
          ^.cls := "header item",
          ^.onClick ==> goHome(p),
          s"Seqexec - ${p.site.shortName}"
        ),
        HeaderItem(HeaderItem.Props(OcsBuildInfo.version, sub = true)),
        wsConnect(ConnectionState.apply),
        userConnect(ControlMenu.apply)
      )
    )
    .componentDidMount(ctx =>
      Callback {
        // Mount the Semantic component using jQuery
        import org.querki.jquery.$
        import web.client.facades.semanticui.SemanticUIVisibility._

        // Pick the top bar and make it stay visible regardless of scrolling
        ctx.getDOMNode.foreach { dom => $(dom).visibility(JsVisiblityOptions.visibilityType("fixed").offset(0)) }
      }
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
  * Alert message when the connection disappears
  */
object ConnectionState {

  final case class Props(u: WebSocketConnection)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def formatTime(delay: Int): String = if (delay < 1000) {
    f"${delay / 1000.0}%.1f"
  } else {
    f"${delay / 1000}%d"
  }

  private val component = ScalaComponent.builder[Props]("ConnectionState")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui header item sub",
        p.u.ws.renderPending(t =>
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

  def apply(u: ModelProxy[WebSocketConnection]): Unmounted[Props, Unit, Unit] = component(Props(u()))
}
