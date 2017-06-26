package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import diode.react.ReactPot._
import edu.gemini.seqexec.web.client.model.{NavigateTo, SeqexecCircuit, WebSocketConnection}
import edu.gemini.seqexec.web.client.model.Pages.Root
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import edu.gemini.seqexec.web.client.OcsBuildInfo
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.menu.HeaderItem
import japgolly.scalajs.react.component.Scala.Unmounted

import scalacss.ScalaCssReact._

/**
  * Component for the bar at the top of the page
  */
object NavBar {
  private val userConnect = SeqexecCircuit.connect(SeqexecCircuit.status)
  private val wsConnect = SeqexecCircuit.connect(_.ws)

  private def goHome(e: ReactEvent): Callback = {
    e.preventDefault
    Callback(SeqexecCircuit.dispatch(NavigateTo(Root)))
  }

  private val component = ScalaComponent.builder[Unit]("SeqexecAppBar")
    .stateless
    .render(_ =>
      <.div(
        SeqexecStyles.mainContainer,
        <.div(
          ^.cls := "ui container five column stackable grid",
          <.div(
            ^.cls := "ui row",
            HeaderItem(HeaderItem.Props(name = ""),
              <.a(
                ^.href := "/#",
                <.img(
                  ^.cls := "ui mini image logo",
                  SeqexecStyles.logo,
                  ^.src :="/images/launcher.png"
                ),
                ^.onClick ==> goHome
              ),
              "Seqexec"
            ),
            HeaderItem(HeaderItem.Props(OcsBuildInfo.version, sub = true)),
            wsConnect(ConnectionState.apply),
            userConnect(TopMenu.apply)
          )
        )
      )
    )
    .componentDidMount(ctx =>
      Callback {
        // Mount the Semantic component using jQuery
        import org.querki.jquery.$

        // Pick the top bar and make it stay visible regardless of scrolling
        $(ctx.getDOMNode).visibility(JsVisiblityOptions.visibilityType("fixed").offset(0))
      }
    )
    .build

  def apply(): Unmounted[Unit, Unit, Unit] = component()
}

/**
  * Alert message when the connection disappears
  */
object ConnectionState {

  case class Props(u: WebSocketConnection)

  def formatTime(delay: Long): String = if (delay < 1000) {
    f"${delay / 1000.0}%.1f"
  } else {
    f"${delay / 1000}%d"
  }

  private val component = ScalaComponent.builder[Props]("ConnectionState")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "header item",
        p.u.ws.renderPending(t =>
          <.div(
            IconAttention.copyIcon(size = Size.Large, color = Option("red")),
            <.span(
              SeqexecStyles.errorText,
              s"Connection lost, retrying in ${formatTime(p.u.nextAttempt)} [s] ..."
            )
          )
        )
      )
    )
    .build

  def apply(u: ModelProxy[WebSocketConnection]) = component(Props(u()))
}
