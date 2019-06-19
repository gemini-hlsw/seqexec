// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.checkbox

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.vdom.html_<^._
import react.common.style._
import react.common.implicits._

/**
  * Semantic UI checkbox componnet
  */
object Checkbox {
  type Backend        = RenderScope[Props, State, Unit]
  type ChangeCallback = Boolean => Callback

  final case class Props(label:       String,
                         checked:     Boolean,
                         onChange:    ChangeCallback = _ => Callback.empty,
                         extraStyles: List[Css] = Nil)

  final case class State(checked: Boolean) {
    def flip: State = copy(checked = !checked)
  }

  private def onChange(b: Backend, ccb: ChangeCallback): Callback =
    // Note in the callback call we haven't yet flipped the flag on the state yet
    b.modState(_.flip) *> ccb(!b.state.checked)

  private def component =
    ScalaComponent
      .builder[Props]("Checkbox")
      .initialStateFromProps(p => State(p.checked))
      .renderC((b, c) =>
        <.div(
          ^.cls := "ui checkbox",
          (^.cls := "checked").when(b.state.checked),
          b.props.extraStyles,
          <.input.checkbox(
            ^.onChange --> onChange(b, b.props.onChange),
            ^.checked := b.state.checked
          ),
          <.label(b.props.label),
          c
      ))
      .build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, State, Unit] =
    component(p)(children: _*)

}
