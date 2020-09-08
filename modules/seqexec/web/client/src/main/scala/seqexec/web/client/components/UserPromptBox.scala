// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.js.JSConverters._
import seqexec.web.client.actions.CloseUserPromptBox
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model._
import seqexec.model.UserPrompt._
import seqexec.web.client.reusability._
import react.common.ReactProps
import react.semanticui.addons.confirm.Confirm
import react.semanticui.elements.button.Button
import react.semanticui.colors._
import react.semanticui.modules.modal.ModalContent
import react.semanticui.modules.modal.ModalSize

final case class UserPromptBox(prompt: UserPromptState)
    extends ReactProps[UserPromptBox](UserPromptBox.component)

/**
 * UI for generic user prompts
 */
object UserPromptBox {
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
        header = not.foldMap(_.title),
        size = ModalSize.Tiny,
        content =
          ModalContent()(<.div(not.map(c => c.question.map(<.p(SeqexecStyles.ConfirmLine, _)).toTagMod).getOrElse(EmptyVdom))),
        open = not.isDefined,
        onCancel = cancel,
        onConfirm = ok,
        cancelButton = Button(content = not.foldMap(_.cancelButton),
                              color = not.flatMap(_.cancelColor.suiColor).orUndefined
        ),
        confirmButton = Button(content = not.foldMap(_.okButton),
                               color = not.flatMap(_.okColor.suiColor).orUndefined
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

}
