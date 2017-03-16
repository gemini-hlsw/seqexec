package edu.gemini.seqexec.web.client.semanticui.elements.input

import japgolly.scalajs.react.{Callback, CallbackTo, ReactComponentB, ReactComponentU, ReactEventI, TopNode}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.extra.ExternalVar

import scalaz.syntax.equal._
import scalaz.std.string._

/**
 * Input component that uses a EVar to share the content of the field
 */
object InputEV {
  type ChangeCallback = String => Callback

  case class Props(name: String,
                   id: String,
                   value: ExternalVar[String],
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
  private val ST = ReactS.Fix[State]

  def onTextChange(p: Props)(e: ReactEventI): ReactST[CallbackTo, State, Unit] = {
    // Capture the value outside setState, react reuses the events
    val v = e.target.value
    // First update the internal state, then call the outside listener
    ST.set(State(v, changed = true)).liftCB >> ST.retM(p.onChange(v)) >> ST.retM(p.value.set(v))
  }

  def onBlur(c: ChangeCallback): ReactST[CallbackTo, State, Unit] =
    ST.get.liftCB.flatMap(v => ST.retM(c(v.value)))

  private val component = ReactComponentB[Props]("InputEV")
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
        ^.onChange ==> $._runState(onTextChange(p)),
        ^.onBlur   --> $.runState(onBlur(p.onBlur))
      )
    }.componentWillMount { $ =>
      // Update state of the input if the property has changed
      Callback.when(($.props.value.value =/= $.state.value) && !$.state.changed)($.setState(State($.props.value.value)))
    }.componentWillReceiveProps { f =>
      // Update state of the input if the property has changed
      Callback.when((f.nextProps.value.value =/= f.$.state.value) && !f.$.state.changed)(f.$.setState(State(f.nextProps.value.value)))
    }.build

  def apply(p: Props): ReactComponentU[Props, State, Unit, TopNode] = component(p)
}
