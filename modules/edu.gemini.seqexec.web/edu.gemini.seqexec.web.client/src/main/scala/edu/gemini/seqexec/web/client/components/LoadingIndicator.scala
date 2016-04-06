package edu.gemini.seqexec.web.client.components

import diode.data.Pot
import diode.react.ReactPot._
import diode.react.ModelProxy
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

class LoadingIndicator[A] private (text: String) {
  val component = ReactComponentB[ModelProxy[Pot[A]]]("LoadingIndicator")
    .stateless
    .render_P(p =>
      <.div(
        p().renderPending(_ => <.div(
          ^.cls := "ui active dimmer",
          <.div(
            ^.cls := "ui text loader large",
            text)
        )
        )
      )
    )
    .build

}

object LoadingIndicator {
  def apply[A](text: String, p: ModelProxy[Pot[A]]) = new LoadingIndicator(text).component(p)
}
