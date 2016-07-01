package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, ToggleDevConsole}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import org.scalajs.dom._

import scalacss.ScalaCssReact._

/**
  * Component for the bar at the top of the page
  */
object NavBar {
  val userConnect = SeqexecCircuit.connect(_.user)
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
  * Menu at the top bar
  */
object ConnectionState {

  case class Props(u: Option[WebSocket])

  val component = ReactComponentB[Props]("ConnectionState")
    .stateless
    .render_P( p =>
      p.u.fold(
        <.div(
          ^.cls := "header item",
          IconAttention.copyIcon(size = Size.Large, color = Option("red")),
          <.span(
            SeqexecStyles.errorText,
            "Connection lost, retrying..."
          )
        ): ReactElement)
      (_ => <.div(): ReactElement)
    )
    .build

  def apply(u: ModelProxy[Option[WebSocket]]) = component(Props(u()))
}
