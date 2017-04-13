package edu.gemini.seqexec.web.client.components

import diode.data.Pot
import diode.react.ReactPot._
import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{SequenceId, SequencesQueue}
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconCircleNotched, IconPlus, IconRemove, IconSearch}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html.Div

import scalacss.ScalaCssReact._

/**
  * Header of the search area, it shows the amount of items found
  */
object LoadSequenceResultsHeader {
  def closeArea = Callback { SeqexecCircuit.dispatch(CloseSearchArea) }

  private val component = ReactComponentB[ModelProxy[Pot[SequencesQueue[SequenceId]]]]("SequenceSearchResultHeader")
    .render_P(p =>
      <.div(
        ^.cls := "ui top attached segment header",
        p().renderPending(_ => <.div(IconCircleNotched.copyIcon(loading = true), "Loading...")),
        p().renderReady(u => <.div(IconRemove.copyIcon(link = true, onClick = closeArea), u.queue match {
          case Nil => "No sequences found"
          case x   => s"Loaded ${x.size} sequence(s)"
        })),
        p().renderFailed(e => <.span(SeqexecStyles.errorText, "Got an error during load (bad id?)"))
      )
    )
    .build

  def apply(searchResults: ModelProxy[Pot[SequencesQueue[SequenceId]]]): ReactComponentU[ModelProxy[Pot[SequencesQueue[SequenceId]]], Unit, Unit, TopNode] = component(searchResults)
}

/**
  * Shows a table with search results
  */
object SequenceLoad {
  private val searchResultsConnect = SeqexecCircuit.connect(_.searchResults)

  private val component = ReactComponentB[Unit]("SequenceSearchResult")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "six wide computer tablet fourteen wide mobile column right floated",
        <.div(
          ^.cls := "segment", {
            implicit val eq = PotEq.searchResultsEq
            searchResultsConnect(LoadSequenceResultsHeader.apply)
          }
        )
      )
    )
    .build

  def apply(): ReactComponentU[Unit, Unit, Unit, TopNode] = component()
}

/**
  * Search field, it lets the user search for obs ids
  */
object SequenceSearch {
  case class Props(model: ModelProxy[(ClientStatus, Pot[SequencesQueue[SequenceId]])]) {
    def searchResults: Pot[SequencesQueue[SequenceId]] = model()._2
    def status: ClientStatus = model()._1
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

    def render(p: Props, s: State): ReactTagOf[Div] =
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

  def apply(p: ModelProxy[(ClientStatus, Pot[SequencesQueue[SequenceId]])]): ReactComponentU[Props, State, Backend, TopNode] = component(Props(p))
}
