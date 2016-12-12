package edu.gemini.seqexec.web.client.components

import diode.data.Pot
import diode.react.ReactPot._
import diode.react.ModelProxy
import edu.gemini.seqexec.model.SharedModel.{SequenceId, SequencesQueue}
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconCircleNotched, IconPlus, IconRemove, IconSearch}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode

import scalacss.ScalaCssReact._

/**
  * Header of the search area, it shows the amount of items found
  */
object SequenceSearchResultsHeader {
  def closeArea = Callback { SeqexecCircuit.dispatch(CloseSearchArea) }

  val component = ReactComponentB[ModelProxy[Pot[SequencesQueue[SequenceId]]]]("SequenceSearchResultHeader")
    .render_P(p =>
      <.div(
        ^.cls := "ui top attached segment header",
        p().renderPending(_ => <.div(IconCircleNotched.copyIcon(loading = true), "Searching...")),
        p().renderReady(u => <.div(IconRemove.copyIcon(link = true, onClick = closeArea), s"Found ${u.queue.size} sequence(s)")),
        p().renderFailed(e => <.span(SeqexecStyles.errorText, "Got an error during search"))
      )
    )
    .build

  def apply(searchResults: ModelProxy[Pot[SequencesQueue[SequenceId]]]) = component(searchResults)
}

/**
  * Displays the results of the search
  * NOTE This component is not in use at the moment
  */
object SequenceSearchResultsBody {
  case class Props(model: ModelProxy[(ClientStatus, Pot[SequencesQueue[SequenceId]])]) {
    def searchResults = model()._2
    def status = model()._1
  }

  def addToQueue[A](p: ModelProxy[A], u: SequenceId): Callback = Callback.empty

  def removeFromResults[A](p: ModelProxy[A], u: SequenceId):Callback = Callback.empty

  def onAdding[A](p: ModelProxy[A], u: SequenceId):Callback = addToQueue(p, u) >> removeFromResults(p, u)

  val component = ReactComponentB[Props]("SequenceSearchResultBody")
    .stateless
    .render_P(p =>
      <.tbody(
        p.searchResults.renderReady(s => s.queue.zipWithIndex.collect { case (u, i) =>
            <.tr(
              ^.key := i,
              <.td(
                ^.cls := "collapsing",
                u
              ),
              <.td(
                ^.cls := "collapsing",
                Button(Button.Props(icon = Some(IconPlus), circular = true,
                  onClick = onAdding(p.model, u), disabled = !p.status.isConnected))
              )
            )
          }
        )
      )
    )
    .build

  def apply(p: ModelProxy[(ClientStatus, Pot[SequencesQueue[SequenceId]])]) = component(Props(p))
}

/**
  * Shows a table with search results
  */
object SequenceSearchResults {
  val statusAndSearchResultsConnect = SeqexecCircuit.connect(SeqexecCircuit.statusAndSearchResults)
  val searchResultsConnect = SeqexecCircuit.connect(_.searchResults)

  val component = ReactComponentB[Unit]("SequenceSearchResult")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "six wide computer tablet fourteen wide mobile column right floated",
        <.div(
          ^.cls := "segment", {
            implicit val eq = PotEq.searchResultsEq
            searchResultsConnect(SequenceSearchResultsHeader.apply)
          },
          <.div(
            ^.cls := "ui scroll pane bottom attached segment",
            SeqexecStyles.searchResultListPane,
            <.table(
              ^.cls := "ui selectable compact table unstackable",
              <.thead(
                <.tr(
                  <.th("Obs ID"),
                  <.th("\u00a0")
                )
              ),
              statusAndSearchResultsConnect(SequenceSearchResultsBody.apply)
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
  case class Props(model: ModelProxy[(ClientStatus, Pot[SequencesQueue[SequenceId]])]) {
    def searchResults = model()._2
    def status = model()._1
  }

  case class State(searchText: String)

  class Backend($: BackendScope[Props, State]) {
    def onEnter(e: ReactKeyboardEventI): Callback = CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Enter => search
      }

    def openResultsArea: Callback =
      $.props >>= { _.model.dispatchCB(OpenSearchArea) }

    def attemptLoad: Callback =
      $.props.zip($.state) >>= { case (p, s) => p.model.dispatchCB(LoadSequence(s.searchText)) }

    def search: Callback =
      openResultsArea >> attemptLoad

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
            ^.placeholder := "Load...",
            ^.onKeyDown ==> onEnter,
            ^.onChange ==> onChange,
            ^.value := s.searchText,
            ^.disabled := !p.status.isConnected
          ),
          IconSearch.copy(Icon.Props(id = IconSearch.p.id, link = true, onClick = search))
        )
      )
  }

  val component = ReactComponentB[Props]("SequenceSearch")
    .initialState(State(""))
    .renderBackend[Backend]
    .build.withKey("key.sequence.search")

  def apply(p: ModelProxy[(ClientStatus, Pot[SequencesQueue[SequenceId]])]) = component(Props(p))
}
