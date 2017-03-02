package edu.gemini.seqexec.web.client.semanticui.elements.input

import japgolly.scalajs.react.{Callback, CallbackTo, ReactComponentB, ReactEventI}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._

import scalaz.syntax.equal._
import scalaz.std.string._

object Input {
  type ChangeCallback = String => Callback

  case class Props(name: String,
                   id: String,
                   value: String,
                   inputType: InputType = TextInput,
                   placeholder: String = "",
                   disabled: Boolean = false,
                   onChange: ChangeCallback = s => Callback.empty, // callback for parents of this component
                   onBlur: ChangeCallback = s => Callback.empty)

  case class State(value: String, changed: Boolean = false)

  sealed trait InputType
  case object TextInput extends InputType
  case object PasswordInput extends InputType

  // Use state monad to hold the state of the system
  val ST = ReactS.Fix[State]

  def onTextChange(c: ChangeCallback)(e: ReactEventI): ReactST[CallbackTo, State, Unit] = {
    // Capture the value outside setState, react reuses the events
    val v = e.target.value
    // First update the internal state, then call the outside listener
    ST.set(State(v, true)).liftCB >> ST.retM(c(v))
  }

  def onBlur(c: ChangeCallback): ReactST[CallbackTo, State, Unit] =
    ST.get.liftCB.flatMap(v => ST.retM(c(v.value)))

  def component = ReactComponentB[Props]("Icon")
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
        ^.onChange ==> $._runState(onTextChange(p.onChange)),
        ^.onBlur   --> $.runState(onBlur(p.onBlur))
      )
    }.componentWillMount { $ =>
      // Update state of the input if the property has changed
      Callback.when(($.props.value /== $.state.value) && !$.state.changed)($.setState(State($.props.value, false)))
    }.build

  def apply(p: Props) = component(p)
}
