// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.forms

import cats.implicits._
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ Callback, ReactEventFromInput, ScalaComponent }
import monocle.Iso
import monocle.Prism
import react.common.ReactProps

/**
  * Input component that uses a EVar to share the content of the field
  */
final case class InputEV[A](
  name:        String,
  id:          String,
  snapshot:    StateSnapshot[A],
  prism:       Prism[A, String] = Iso.id[String].asPrism,
  inputType:   InputEV.InputType = InputEV.TextInput,
  placeholder: String = "",
  disabled:    Boolean = false,
  onChange:    InputEV.ChangeCallback[A] = (_: A) => Callback.empty, // callback for parents of this component
  onBlur:      InputEV.ChangeCallback[A] = (_: A) => Callback.empty
) extends ReactProps {
  @inline def render: VdomElement = InputEV.component(this)
  def valGet: String              = prism.getOption(snapshot.value).orEmpty
  def valSet(s: String): Callback = snapshot.setState(prism.reverseGet(s))
  val onBlurC: InputEV.ChangeCallback[String] =
    (s: String) => onBlur(prism.reverseGet(s))
  val onChangeC: InputEV.ChangeCallback[String] =
    (s: String) => onChange(prism.reverseGet(s))
}

object InputEV {
  type Props             = InputEV[_]
  type ChangeCallback[A] = A => Callback
  type Backend           = RenderScope[Props, State, Unit]

  final case class State(curValue: Option[String], changed: Boolean = false)

  sealed trait InputType extends Product with Serializable
  case object TextInput extends InputType
  case object PasswordInput extends InputType

  def onTextChange(b: Backend)(e: ReactEventFromInput): Callback = {
    // Capture the value outside setState, react reuses the events
    val v = e.target.value
    // First update the internal state, then call the outside listener
    b.setState(State(v.some, changed = true)) *>
      b.props.valSet(v) *>
      b.props.onChangeC(v)
  }

  def onBlur(b: Backend, c: ChangeCallback[String]): Callback =
    c(b.state.curValue.orEmpty)

  protected val component =
    ScalaComponent
      .builder[Props]("InputEV")
      .initialState(State(None))
      .render { b =>
        val p = b.props
        val s = b.state
        <.input(
          ^.`type` := (p.inputType match {
            case TextInput     => "text"
            case PasswordInput => "password"
          }),
          ^.placeholder := p.placeholder,
          ^.name := p.name,
          ^.id := p.id,
          ^.value := s.curValue.orEmpty,
          ^.disabled := p.disabled,
          ^.onChange ==> onTextChange(b),
          ^.onBlur --> onBlur(b, p.onBlurC)
        )
      }
      .componentWillMount { ctx =>
        // Update state of the input if the property has changed
        ctx
          .setState(State(ctx.props.valGet.some))
          .when_((ctx.props.valGet.some =!= ctx.state.curValue) && !ctx.state.changed)
      }
      .componentWillReceiveProps { ctx =>
        // Update state of the input if the property has changed
        // TBD Should check if the state has changed?
        ctx
          .setState(State(ctx.nextProps.valGet.some))
          .when_(ctx.nextProps.valGet.some =!= ctx.state.curValue)
      }
      .build
}

