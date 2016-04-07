package edu.gemini.seqexec.web.client.components

import diode.FastEq
import diode.data.Pot
import diode.react.ModelProxy
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

class LoadingIndicator[A] private (text: String) {

  val component = ReactComponentB[ModelProxy[Pot[A]]]("LoadingIndicator")
    .render_P(p =>
      <.div(
        ^.cls := "ui dimmer",
        ^.classSet(
          "active" -> p().isPending
        ),
        <.div(
          ^.cls := "ui text loader large",
          text
        )
      )
    )
    .build

}

object LoadingIndicator {
  def apply[A](text: String, p: ModelProxy[Pot[A]])(implicit feq: FastEq[_ >: Pot[A]]) = new LoadingIndicator(text).component(p)
}
