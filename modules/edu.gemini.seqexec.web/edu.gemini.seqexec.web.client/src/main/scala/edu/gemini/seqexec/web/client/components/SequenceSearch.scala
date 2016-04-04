package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

object SequenceSearch {
  case class State(searchText: String)

  class Backend($: BackendScope[Unit, State]) {
    def onEnter(e: ReactKeyboardEventI): Callback = Callback.ifTrue(e.charCode == 13, search)

    def search: Callback = $.state >>= {s => Callback.log("search " + s.searchText)}

    def onChange(e: ReactEventI): Callback =
      // For some reason the simple call $.modState(_.copy(searchText = e.target.value)) gives an NPE on e.target
      // Possibly a react.js fault, but the code below does the trick
      CallbackTo.pure(e.target.value) >>= {e => $.modState(_.copy(searchText = e))}

    def render(s: State) =
      <.div(
        ^.cls := "ui right aligned category search item",
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
          Icon(Icon.Props("search link"))
        ),
        <.div(
          ^.cls := "results"
        )
      )
  }

  val component = ReactComponentB[Unit]("SequenceSearch")
    .initialState(State(""))
    .renderBackend[Backend]
    .buildU

  def apply() = component()
}
