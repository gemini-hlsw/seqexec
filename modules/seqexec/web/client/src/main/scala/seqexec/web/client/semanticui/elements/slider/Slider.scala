// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.slider

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalazReact._
import web.client.style._

/**
 * Semantic UI slider componnet
 */
object Slider {
  type ChangeCallback = Boolean => Callback
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(label: String,
                   checked    : Boolean,
                   onChange   : ChangeCallback = _ => Callback.empty,
                   extraStyles: List[GStyle] = Nil)

  final case class State(checked: Boolean) {
    def flip: State = copy(checked = !checked)
  }

  private val ST = ReactS.Fix[State]

  private def onChange(c: ChangeCallback): ReactST[CallbackTo, State, Unit] =
    ST.mod(_.flip).liftCB >> ST.get.liftCB.flatMap {v => ST.retM(c(v.checked))}

  private def component = ScalaComponent.builder[Props]("Slider")
    .initialStateFromProps(p => State(p.checked))
    .renderPCS(($, p, c, s) =>
      <.div(
        ^.cls := "ui slider checkbox",
        (^.cls := "checked").when(s.checked),
        p.extraStyles.map(geminiStyleToTagMod).toTagMod,
        <.input.checkbox(
          ^.onChange --> $.runState(onChange(p.onChange)),
          ^.checked := s.checked
        ),
        <.label(p.label),
        c
      )
    ).build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, State, Unit] = component(p)(children: _*)

}
