package edu.gemini.seqexec.web.client.components

import diode.FastEq
import diode.data.Pot
import diode.react.ReactPot._
import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model.{SearchAreaOpen, SearchSequence, SeqexecCircuit}
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.common.Sequence
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode

object SequenceSearchResultsHeader {
  val component = ReactComponentB[ModelProxy[Pot[List[Sequence]]]]("SequenceSearchResultHeader")
    .render_P(p =>
      <.div(
        ^.cls := "ui top attached segment header",
        s"Found ${p().map(_.size).getOrElse(0)}"
      )
    )
    .build

  def apply(searchResults: ModelProxy[Pot[List[Sequence]]]) = component(searchResults)
}

object SequenceSearchResultsBody{
  val component = ReactComponentB[ModelProxy[Pot[List[Sequence]]]]("SequenceSearchResultBody")
    .render_P(p =>
      <.tbody(
        p().renderReady(s => s.zipWithIndex.collect { case (u, i) =>
            <.tr(
              ^.key := i,
              <.td(
                ^.cls := "collapsing",
                u.id
              ),
              <.td("GPI"),
              <.td(
                ^.cls := "collapsing",
                <.button(
                  ^.cls := "circular ui icon button",
                  Icon("plus")
                )
              )
            )
          }
        )
      )
    )
    .build

  def apply(searchResults: ModelProxy[Pot[List[Sequence]]]) = component(searchResults)
}

object SequenceSearchResults {
  implicit object ExtDataEq extends FastEq[Pot[List[Sequence]]] {
    override def eqv(a: Pot[List[Sequence]], b: Pot[List[Sequence]]): Boolean = a.state == b.state
  }

  val component = ReactComponentB[Unit]("SequenceSearchResult")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "six wide column",
        SeqexecCircuit.connect(_.searchResults)(LoadingIndicator("Searching...", _)),
        SeqexecCircuit.connect(_.searchResults)(SequenceSearchResultsHeader.apply),
        <.div(
          ^.cls := "ui scroll pane attached segment",
          <.table(
            ^.cls := "ui selectable compact table unstackable",
            <.thead(
              <.tr(
                <.th("Obs ID"),
                <.th("Instrument"),
                <.th("\u00a0")
              )
            ),
            SeqexecCircuit.connect(_.searchResults)(SequenceSearchResultsBody.apply)
          )
        )
      )
    )
    .buildU

  def apply() = component()
}
object SequenceSearch {
  case class Props(searchResults: ModelProxy[Pot[List[Sequence]]])

  case class State(searchText: String)

  class Backend($: BackendScope[Props, State]) {
    def onEnter(e: ReactKeyboardEventI): Callback = Callback.ifTrue(e.charCode == KeyCode.Enter, search)

    def search: Callback =
      //$.props.zip($.state) >>= {case (p, s) => p.searchResults.dispatch(SearchSequence(s.searchText))}
      $.props.zip($.state) >>= {case (p, s) => p.searchResults.dispatch(SearchAreaOpen)}

    def onChange(e: ReactEventI): Callback =
      // For some reason the simple call $.modState(_.copy(searchText = e.target.value)) gives an NPE on e.target
      // Possibly a react.js fault, but the code below does the trick
      CallbackTo.pure(e.target.value) >>= {e => $.modState(_.copy(searchText = e))}

    def render(p: Props, s: State) =
      <.div(
        ^.cls := "ui right aligned category search dropdown item",
        <.div(
          ^.cls := "ui transparent icon input",
          <.input(
            ^.cls := "prompt",
            ^.`type` := "text",
            ^.placeholder := "Search...",
            ^.onKeyPress ==> onEnter,
            ^.onChange ==> onChange,
            ^.value := s.searchText
          ),
          Icon(Icon.Props("search", link = true, onClick = search))
        ),
        <.div(
          ^.cls := "menu"
          // TODO Add elements of the queue as <div class="item">text</div>
        )
      )
  }

  val component = ReactComponentB[Props]("SequenceSearch")
    .initialState(State(""))
    .renderBackend[Backend]
    .componentDidMount(s =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$

        $(ReactDOM.findDOMNode(s)).dropdown()
      }
    )
    .build

  def apply(searchResults: ModelProxy[Pot[List[Sequence]]]) = component(Props(searchResults))
}
