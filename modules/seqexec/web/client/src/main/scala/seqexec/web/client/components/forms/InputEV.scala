// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.forms

import cats.implicits._
import japgolly.scalajs.react.{Callback, ReactEventFromInput, ScalaComponent}
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import react.common.ReactProps

/**
 * Input component that uses a EVar to share the content of the field
 */
final case class InputEV(
  name:        String,
  id:          String,
  value:       StateSnapshot[String],
  inputType:   InputEV.InputType = InputEV.TextInput,
  placeholder: String = "",
  disabled:    Boolean = false,
  onChange:    InputEV.ChangeCallback = _ => Callback.empty, // callback for parents of this component
  onBlur:      InputEV.ChangeCallback = _ => Callback.empty
) extends ReactProps {
  @inline def render: VdomElement = InputEV.component(this)
}

object InputEV {
  type Props = InputEV

  type ChangeCallback = String => Callback
  type Backend      = RenderScope[Props, State, Unit]

  final case class State(value: String, changed: Boolean = false)

  sealed trait InputType
  case object TextInput extends InputType
  case object PasswordInput extends InputType

  def onTextChange(b: Backend)(e: ReactEventFromInput): Callback = {
    // Capture the value outside setState, react reuses the events
    val v = e.target.value
    // First update the internal state, then call the outside listener
    b.setState(State(v, changed = true)) *>
    b.props.value.setState(v) *>
    b.props.onChange(v)
  }

  def onBlur(b: Backend, c: ChangeCallback): Callback =
    c(b.state.value)

  protected val component = ScalaComponent.builder[Props]("InputEV")
    .initialState(State(""))
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
        ^.value := s.value,
        ^.disabled := p.disabled,
        ^.onChange ==> onTextChange(b),
        ^.onBlur   --> onBlur(b, p.onBlur)
      )
    }.componentWillMount { ctx =>
      // Update state of the input if the property has changed
      Callback.when((ctx.props.value.value =!= ctx.state.value) && !ctx.state.changed)(ctx.setState(State(ctx.props.value.value)))
    }.componentWillReceiveProps { ctx =>
      // Update state of the input if the property has changed
      // TBD Should check if the state has changed?
      Callback.when(ctx.nextProps.value.value =!= ctx.state.value)(ctx.setState(State(ctx.nextProps.value.value)))
    }.build
}