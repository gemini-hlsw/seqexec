package edu.gemini.seqexec.web.client.components

import diode.data.Pot
import diode.react.ReactPot._
import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconPlus, IconSearch, IconCircleNotched, IconRemove}
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
        p().renderPending(_ => <.div(IconCircleNotched.copyIcon(loading = true), "Searching...")),
        p().renderReady(u => <.div(IconRemove.copyIcon(link = true, onClick = Callback {SeqexecCircuit.dispatch(CloseSearchArea)}), s"Found ${u.size} sequence(s)")),
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
  case class Props(searchResults: ModelProxy[Pot[List[Sequence]]])

  def addToQueue[A](p: ModelProxy[A], u: Sequence):Callback = p.dispatch(AddToQueue(u))

  def removeFromResults[A](p: ModelProxy[A], u: Sequence):Callback = p.dispatch(RemoveFromSearch(u))

  def onAdding[A](p: ModelProxy[A], u: Sequence):Callback = addToQueue(p, u) >> removeFromResults(p, u)

  val component = ReactComponentB[Props]("SequenceSearchResultBody")
    .stateless
    .render_P(p =>
      <.tbody(
        p.searchResults().renderReady(s => s.zipWithIndex.collect { case (u, i) =>
            <.tr(
              ^.key := i,
              <.td(
                ^.cls := "collapsing",
                u.id
              ),
              <.td(u.instrument),
              <.td(
                ^.cls := "collapsing",
                Button(Button.Props(icon = Some(IconPlus), circular = true, onClick = onAdding(p.searchResults, u)))
              )
            )
          }
        )
      )
    )
    .build

  def apply(searchResults: ModelProxy[Pot[List[Sequence]]]) = component(Props(searchResults))
}

/**
  * Shows a table with search results
  */
object SequenceSearchResults {

  val component = ReactComponentB[Unit]("SequenceSearchResult")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "six wide computer tablet fourteen wide mobile column right floated",
        <.div(
          ^.cls := "segment", {
            implicit val eq = PotEq.searchResultsEq
            SeqexecCircuit.connect(_.searchResults)(SequenceSearchResultsHeader.apply)
          },
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
    .build

  def apply() = component()
}

/**
  * Search field, it lets the user search for obs ids
  */
object SequenceSearch {
  case class Props(searchResults: ModelProxy[Pot[List[Sequence]]])

  case class State(searchText: String)

  class Backend($: BackendScope[Props, State]) {
    def onEnter(e: ReactKeyboardEventI): Callback = CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Enter => search
      }

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
        ^.cls := "ui right aligned category item",
        <.div(
          ^.cls := "ui focus icon input",
          <.input(
            ^.`type` := "text",
            ^.placeholder := "Search...",
            ^.onKeyDown ==> onEnter,
            ^.onChange ==> onChange,
            ^.value := s.searchText
          ),
          IconSearch.copy(Icon.Props(id = IconSearch.p.id, link = true, onClick = search))
        )
      )
  }

  val component = ReactComponentB[Props]("SequenceSearch")
    .initialState(State(""))
    .renderBackend[Backend]
    .build

  def apply(searchResults: ModelProxy[Pot[List[Sequence]]]) = component(Props(searchResults))
}
