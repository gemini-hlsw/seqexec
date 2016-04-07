package edu.gemini.seqexec.web.client.components

import diode.data.Pot
import diode.react.ReactPot._
import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.common.Sequence
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode

import scalacss.ScalaCssReact._

/**
  * Header of the search area, it shows the amount of items found
  */
object SequenceSearchResultsHeader {
  val component = ReactComponentB[ModelProxy[Pot[List[Sequence]]]]("SequenceSearchResultHeader")
    .render_P(p =>
      <.div(
        ^.cls := "ui top attached segment header",
        p().renderEmpty("Sequence Search"),
        p().renderPending(_ => "Searching..."),
        p().render(u => s"Found ${u.size} sequence(s)"),
        p().renderFailed(e => <.span(SeqexecStyles.errorText, "Got an error during search"))
      )
    )
    .build

  def apply(searchResults: ModelProxy[Pot[List[Sequence]]]) = component(searchResults)
}

/**
  * Displays the results of the search
  */
object SequenceSearchResultsBody {
  def addToQueue[A](p: ModelProxy[A], u: Sequence):Callback = p.dispatch(AddToQueue(u))

  def removeFromResults[A](p: ModelProxy[A], u: Sequence):Callback = p.dispatch(RemoveFromSearch(u))

  def onAdding[A](p: ModelProxy[A], u: Sequence):Callback = addToQueue(p, u) >> removeFromResults(p, u)

  val component = ReactComponentB[ModelProxy[Pot[List[Sequence]]]]("SequenceSearchResultBody")
    .stateless
    .render_P(p =>
      <.tbody(
        p().renderReady(s => s.zipWithIndex.collect { case (u, i) =>
            <.tr(
              ^.key := i,
              <.td(
                ^.cls := "collapsing",
                u.id
              ),
              <.td(u.instrument),
              <.td(
                ^.cls := "collapsing",
                <.button(
                  ^.cls := "circular ui icon button",
                  Icon(Icon.Props("plus", onClick = onAdding(p, u)))
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

/**
  * Shows a table with search results
  */
object SequenceSearchResults {

  val component = ReactComponentB[Unit]("SequenceSearchResult")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "six wide column", {
          implicit val eq = PotEq.searchResultsEq
          SeqexecCircuit.connect(_.searchResults)(LoadingIndicator("Searching...", _))
        },
        <.div(
          ^.cls := "segment",
          SeqexecCircuit.connect(_.searchResults)(SequenceSearchResultsHeader.apply),
          <.div(
            ^.cls := "ui scroll pane bottom attached segment",
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
    )
    .buildU

  def apply() = component()
}

/**
  * Search field, it lets the user search for obs ids
  */
object SequenceSearch {
  case class Props(searchResults: ModelProxy[Pot[List[Sequence]]])

  case class State(searchText: String)

  class Backend($: BackendScope[Props, State]) {
    def onEnter(e: ReactKeyboardEventI): Callback = Callback.ifTrue(e.charCode == KeyCode.Enter, search)

    def openResultsArea: Callback =
      $.props.zip($.state) >>= {case (p, s) => p.searchResults.dispatch(OpenSearchArea)}

    def startSearch: Callback =
      $.props.zip($.state) >>= {case (p, s) => p.searchResults.dispatch(SearchSequence(s.searchText))}

    def search: Callback =
      openResultsArea >> startSearch

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
