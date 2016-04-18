package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.semanticui._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactDOM, ReactNode}

/**
  * Menu with tabs
  */
object TabularMenu {
  case class TabItem(title: String, isActive: Boolean, dataItem: String)
  case class Props(tabs: List[TabItem])

  val component = ReactComponentB[Props]("TabularMenu")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui attached tabular menu",
        p.tabs.map(t =>
          <.a(
            ^.cls := "item",
            ^.classSet(
              "active" -> t.isActive
            ),
            dataTab := t.dataItem,
            t.title
          )
        )
      )
    ).componentDidMount(s =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import edu.gemini.seqexec.web.client.semanticui.SemanticUI._

        $(ReactDOM.findDOMNode(s)).find(".item").tab()
      }
    ).build

  def apply(tabs: List[TabItem], children: ReactNode*) = component(Props(tabs), children)
}
