package edu.gemini.seqexec.web.client.components

import diode.data.Pot
import diode.react.ReactPot._
import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model.SearchSequence
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.common.Sequence
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode

object SequenceSearch {
  case class Props(searchResults: ModelProxy[Pot[List[Sequence]]])

  case class State(searchText: String)

  class Backend($: BackendScope[Props, State]) {
    def onEnter(e: ReactKeyboardEventI): Callback = Callback.ifTrue(e.charCode == KeyCode.Enter, search)

    def search: Callback = $.state >>= {s => $.props >>= {_.searchResults.dispatch(SearchSequence(s.searchText))}}

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
      ),
        p.searchResults().renderPending(_ => <.p("pending")),
        p.searchResults().renderFailed(e => <.p("failed"))
      )
  }

  val component = ReactComponentB[Props]("SequenceSearch")
    .initialState(State("GS-2016A-Q-0-1"))
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
