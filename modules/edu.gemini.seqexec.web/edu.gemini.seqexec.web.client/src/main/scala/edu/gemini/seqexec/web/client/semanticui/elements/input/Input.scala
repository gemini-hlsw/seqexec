package edu.gemini.seqexec.web.client.semanticui.elements.input

import japgolly.scalajs.react.{Callback, CallbackTo, ReactComponentB, ReactEventI}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._

object Input {
  type ChangeCallback = String => Callback

  case class Props(name: String,
                   id: String,
                   inputType: InputType = TextInput,
                   placeholder: String = "",
                   onChange: ChangeCallback = s => Callback.empty) // callback for parents of this component

  sealed trait InputType
  case object TextInput extends InputType
  case object PasswordInput extends InputType

  // Use state monad to hold the state of the system
  val ST = ReactS.Fix[String]

  def onTextChange(c: ChangeCallback)(e: ReactEventI):ReactST[CallbackTo, String, Unit] = {
    // Capture the value outside setState, react reuses the events
    val v = e.target.value
    // First update the internal state, then call the outside listener
    ST.set(v).liftCB >> ST.retM(c(v))
  }

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
        ^.onChange ==> $._runState(onTextChange(p.onChange))
      )
    }
    .build

  def apply(p: Props) = component(p)
}
