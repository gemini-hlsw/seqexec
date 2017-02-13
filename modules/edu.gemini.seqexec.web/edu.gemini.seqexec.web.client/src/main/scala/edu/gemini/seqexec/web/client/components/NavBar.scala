package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import diode.react.ReactPot._
import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, ToggleDevConsole, WebSocketConnection}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.OcsBuildInfo
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._

import scalacss.ScalaCssReact._

/**
  * Component for the bar at the top of the page
  */
object NavBar {
  val userConnect = SeqexecCircuit.connect(SeqexecCircuit.status)
  val wsConnect = SeqexecCircuit.connect(_.ws)

  val component = ReactComponentB[Unit]("SeqexecAppBar")
    .stateless
    .render(_ =>
      <.div(
        SeqexecStyles.mainContainer,
        <.div(
          ^.cls := "ui container",
          <.div(
            ^.href :="#",
            ^.cls := "ui header item",
            <.img(
              ^.cls := "logo",
              ^.src :="images/launcher.png"
            ),
            "Seqexec"
          ),
          <.div(
            ^.cls := "header item",
            OcsBuildInfo.version
          ),
          TagMod.devOnly(
            <.div(
              ^.cls := "header item",
              IconTerminal.copy(p = IconTerminal.p.copy(link = true, circular = true, onClick = Callback {SeqexecCircuit.dispatch(ToggleDevConsole)}))
            )
          ),
          wsConnect(ConnectionState.apply),
          userConnect(TopMenu.apply)
        )
      )
    )
    .componentDidMount(s =>
      Callback {
        // Mount the Semantic component using jQuery
        import org.querki.jquery.$

        // Pick the top bar and make it stay visible regardless of scrolling
        $(ReactDOM.findDOMNode(s)).visibility(JsVisiblityOptions.visibilityType("fixed").offset(0))
      }
    )
    .build

  def apply() = component()
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

  val component = ReactComponentB[Props]("ConnectionState")
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
