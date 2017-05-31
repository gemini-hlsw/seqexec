package edu.gemini.seqexec.web.client.components

import diode.FastEq
import diode.data.Pot
import diode.react.ModelProxy
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

/**
  * Displays an indicator that something is being done in the background
  */
class LoadingIndicator[A] private (text: String) {

  private val component = ScalaComponent.builder[ModelProxy[Pot[A]]]("LoadingIndicator")
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
  def apply[A](text: String, p: ModelProxy[Pot[A]])(implicit feq: FastEq[_ >: Pot[A]]): Unmounted[ModelProxy[Pot[A]], Unit, Unit] = new LoadingIndicator[A](text).component(p)
}
