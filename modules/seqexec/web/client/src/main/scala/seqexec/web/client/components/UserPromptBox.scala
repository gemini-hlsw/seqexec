// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import scala.scalajs.js.JSConverters._

import cats.syntax.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.addons.confirm.Confirm
import react.semanticui.colors._
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal.ModalContent
import react.semanticui.modules.modal.ModalSize
import seqexec.model.UserPrompt
import seqexec.model.UserPrompt._
import seqexec.web.client.actions.CloseUserPromptBox
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model._
import seqexec.web.client.reusability._

final case class UserPromptBox(prompt: UserPromptState)
    extends ReactProps[UserPromptBox](UserPromptBox.component)

/**
 * UI for generic user prompts
 */
object UserPromptBox {

  def title(n: UserPrompt): String =
    n match {
      case _: TargetCheckOverride => "Warning!"
    }

  def okButton(n: UserPrompt): String =
    n match {
      case _: TargetCheckOverride => "Stop"
    }

  def cancelButton(n: UserPrompt): String =
    n match {
      case _: TargetCheckOverride => "Continue anyway"
    }

  def okColor(n: UserPrompt): PromptButtonColor =
    n match {
      case _: TargetCheckOverride => PromptButtonColor.DefaultOk
    }

  def cancelColor(n: UserPrompt): PromptButtonColor =
    n match {
      case _: TargetCheckOverride => PromptButtonColor.WarningCancel
    }

  def question(n: UserPrompt): List[String] =
    n match {
      case TargetCheckOverride(sid, _, obsTarget, tcsTarget) =>
        List(s"There is a target mismatch running sequence ${sid.format}:",
             s"Target in the sequence: ${obsTarget}",
             s"Target in the TCS: ${tcsTarget}"
        )
    }

  type Props = UserPromptBox

  implicit class PromptButtonColorOps(val c: PromptButtonColor) extends AnyVal {
    def suiColor: Option[SemanticColor] = none
  }

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.prompt)

  private val ok     = Callback(SeqexecCircuit.dispatch(CloseUserPromptBox(UserPromptResult.Ok)))
  private val cancel = Callback(
    SeqexecCircuit.dispatch(CloseUserPromptBox(UserPromptResult.Cancel))
  )

  private val component = ScalaComponent
    .builder[Props]
    .stateless
    .render_P { p =>
      val UserPromptState(not) = p.prompt
      Confirm(
        header = not.foldMap(title(_)),
        size = ModalSize.Tiny,
        content = ModalContent()(
          <.div(
            not
              .map(question(_).map(<.p(SeqexecStyles.ConfirmLine, _)).toTagMod)
              .getOrElse(EmptyVdom)
          )
        ),
        open = not.isDefined,
        onCancel = cancel,
        onConfirm = ok,
        cancelButton = Button(content = not.foldMap(cancelButton),
                              color = not.flatMap(cancelColor(_).suiColor).orUndefined
        ),
        confirmButton = Button(content = not.foldMap(okButton),
                               color = not.flatMap(okColor(_).suiColor).orUndefined
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

}
