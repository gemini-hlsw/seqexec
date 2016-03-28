package edu.gemini.seqexec.web.client.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._

import scalacss.ScalaCssReact._

/**
  * Component for the search form at the top of the page
  */
object NavBar {

  case class Props(searchStart: String => Callback)

  val component = ReactComponentB[Props]("SeqexecAppBar")
    .render(P =>
      <.div(
        SeqexecStyles.mainContainer,
        <.div(
          ^.cls := "ui header container",
          <.div(
            ^.href :="#",
            ^.cls := "header item",
            <.img(
              ^.cls := "logo",
              ^.src :="images/launcher.png"
            ),
            "Seqexec"
          ),
          TopMenu()
        )
      )
    )
    .componentDidMount(s =>
      Callback {
        import org.querki.jquery.$

        $(ReactDOM.findDOMNode(s)).visibility(JsVisiblityOptions.visibilityType("fixed"))
      }
    )
    .build

  def apply(p: Props) = component(p)
}
