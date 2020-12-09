// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.syntax.all._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactMouseEvent
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.colors._
import react.semanticui.elements.button.Button
import react.semanticui.modules.popup.Popup
import seqexec.model.Observation
import seqexec.web.client.actions.RequestRunFrom
import seqexec.web.client.actions.RunOptions
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._
import seqexec.web.client.model.StartFromOperation
import seqexec.web.client.reusability._

/**
 * Contains the control to start a step from an arbitrary point
 */
final case class RunFromStep(
  id:               Observation.Id,
  stepId:           Int,
  resourceInFlight: Boolean,
  runFrom:          StartFromOperation
) extends ReactProps[RunFromStep](RunFromStep.component)

object RunFromStep {
  type Props = RunFromStep

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def requestRunFrom(
    id:     Observation.Id,
    stepId: Int
  ): (ReactMouseEvent, Button.ButtonProps) => Callback =
    (e: ReactMouseEvent, _: Button.ButtonProps) =>
      SeqexecCircuit
        .dispatchCB(RequestRunFrom(id, stepId, RunOptions.Normal))
        .unless_(e.altKey || e.button === StepsTable.MiddleButton)

  protected val component = ScalaComponent
    .builder[Props]("RunFromStep")
    .render_P { p =>
      <.div(
        SeqexecStyles.runFrom,
        SeqexecStyles.notInMobile,
        Popup(
          trigger = Button(
            icon = true,
            color = Blue,
            onClickE = requestRunFrom(p.id, p.stepId),
            disabled = p.resourceInFlight || p.runFrom === StartFromOperation.StartFromInFlight
          )(IconPlay)
        )(s"Run from step ${p.stepId + 1}")
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
