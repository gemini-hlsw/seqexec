// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.input

import japgolly.scalajs.react.{Callback, CallbackTo, ReactEventFromInput, ScalaComponent}
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import cats.implicits._

/**
 * Input component that uses a EVar to share the content of the field
 */
object InputEV {
  type ChangeCallback = String => Callback

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(name: String,
                   id: String,
                   value: StateSnapshot[String],
                   inputType: InputType = TextInput,
                   placeholder: String = "",
                   disabled: Boolean = false,
                   onChange: ChangeCallback = _ => Callback.empty, // callback for parents of this component
                   onBlur: ChangeCallback = _ => Callback.empty)

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class State(value: String, changed: Boolean = false)

  sealed trait InputType
  case object TextInput extends InputType
  case object PasswordInput extends InputType

  // Use state monad to hold the state of the system
  private val ST = ReactS.Fix[State]

  def onTextChange(p: Props)(e: ReactEventFromInput): ReactST[CallbackTo, State, Unit] = {
    // Capture the value outside setState, react reuses the events
    val v = e.target.value
    // First update the internal state, then call the outside listener
    ST.set(State(v, changed = true)).liftCB >> ST.retM(p.value.setState(v)) >> ST.retM(p.onChange(v))
  }

  def onBlur(c: ChangeCallback): ReactST[CallbackTo, State, Unit] =
    ST.get.liftCB.flatMap(v => ST.retM(c(v.value)))

  private val component = ScalaComponent.builder[Props]("InputEV")
    .initialState(State(""))
    .renderPS { ($, p, s) =>
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
        ^.onChange ==> $.runStateFn(onTextChange(p)),
        ^.onBlur   --> $.runState(onBlur(p.onBlur))
      )
    }.componentWillMount { ctx =>
      // Update state of the input if the property has changed
      Callback.when((ctx.props.value.value =!= ctx.state.value) && !ctx.state.changed)(ctx.setState(State(ctx.props.value.value)))
    }.componentWillReceiveProps { ctx =>
      // Update state of the input if the property has changed
      // TBD Should check if the state has changed?
      Callback.when(ctx.nextProps.value.value =!= ctx.state.value)(ctx.setState(State(ctx.nextProps.value.value)))
    }.build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}
