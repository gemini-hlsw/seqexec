package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.model.SequencesOnDisplay
import edu.gemini.seqexec.web.client.semanticui._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactDOM}

/**
  * Menu with tabs
  */
object TabularMenu {
  case class TabItem(title: String, isActive: Boolean, dataItem: String)
  case class Props(tabs: List[TabItem])

  def sequencesTabs(d: SequencesOnDisplay) = d.instrumentSequences.map(a => TabItem(a.instrument, isActive = a == d.instrumentSequences.focus, a.instrument))

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

  def apply(p: SequencesOnDisplay) = component(Props(sequencesTabs(p).toStream.toList))
}
