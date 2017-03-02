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
                   onBlur: ChangeCallback = s => Callback.empty) // callback for parents of this component

  sealed trait InputType
  case object TextInput extends InputType
  case object PasswordInput extends InputType

  // Use state monad to hold the state of the system
  val ST = ReactS.Fix[String]

  def onTextChange(e: ReactEventI): ReactST[CallbackTo, String, Unit] = {
    // Capture the value outside setState, react reuses the events
    val v = e.target.value
    // First update the internal state, then call the outside listener
    ST.set(v).liftCB
  }

  def onBlur(c: ChangeCallback): ReactST[CallbackTo, String, Unit] =
    ST.get.liftCB.flatMap(v => ST.retM(c(v)))

  def component = ReactComponentB[Props]("Icon")
    .initialState("")
    .renderPS { ($, p, s) =>
      <.input(
        ^.`type` := (p.inputType match {
          case TextInput     => "text"
          case PasswordInput => "password"
        }),
        ^.placeholder := p.placeholder,
        ^.name := p.name,
        ^.id := p.id,
        ^.value := s,
        ^.disabled := p.disabled,
        ^.onChange ==> $._runState(onTextChange),
        ^.onBlur   --> $.runState(onBlur(p.onBlur))
      )
    }.componentWillMount { $ =>
      // Update state of the input if the property has changed
      Callback.when($.props.value /== $.state)($.setState($.props.value))
    }.build

  def apply(p: Props) = component(p)
}
