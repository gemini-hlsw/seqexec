package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, ToggleDevConsole}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconTerminal

import scalacss.ScalaCssReact._

/**
  * Component for the bar at the top of the page
  */
object NavBar {

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
          SeqexecCircuit.connect(_.user)(TopMenu(_))
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
