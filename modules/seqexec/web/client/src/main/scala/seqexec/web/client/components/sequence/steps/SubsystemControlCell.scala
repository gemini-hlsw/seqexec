// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactEvent
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.Reusability._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.colors._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.LabelPosition
import react.semanticui.elements.icon.Icon
import react.semanticui.modules.popup.Popup
import react.semanticui.SemanticColor
import react.semanticui.sizes._
import scala.collection.immutable.SortedMap
import scala.scalajs.js
import seqexec.model.enum._
import seqexec.model.StepId
import seqexec.web.client.actions.RequestResourceRun
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.reusability._

/**
  * Contains the control buttons for each subsystem
  */
final case class SubsystemControlCell(
  id:             Observation.Id,
  stepId:         Int,
  resources:      List[Resource],
  resourcesCalls: SortedMap[Resource, ResourceRunOperation],
  canOperate:     Boolean
) extends ReactProps[SubsystemControlCell](SubsystemControlCell.component)

object SubsystemControlCell {
  type Props = SubsystemControlCell

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def requestResourceCall(
    id:     Observation.Id,
    stepId: StepId,
    r:      Resource
  ): (ReactEvent, Button.ButtonProps) => Callback =
    (e: ReactEvent, p: Button.ButtonProps) =>
      Callback.log(p.toString()) *>
        e.preventDefaultCB *> e.stopPropagationCB *>
        SeqexecCircuit.dispatchCB(RequestResourceRun(id, stepId, r))

  private val CompletedIcon = IconCheckmark.copy(
    fitted = true,
    clazz  = SeqexecStyles.completedIcon
  )

  private val RunningIcon = IconCircleNotched.copy(
    fitted  = true,
    loading = true,
    clazz   = SeqexecStyles.runningIcon
  )

  private val FailureIcon = IconAttention.copy(
    fitted   = true,
    inverted = true,
    clazz    = SeqexecStyles.errorIcon
  )

  // We want blue if the resource operation is idle or does not exist: these are equivalent cases.
  private def buttonColor(op: Option[ResourceRunOperation]): SemanticColor =
    op.map {
        case ResourceRunOperation.ResourceRunIdle         => Blue
        case ResourceRunOperation.ResourceRunInFlight(_)  => Yellow
        case ResourceRunOperation.ResourceRunCompleted(_) => Green
        case ResourceRunOperation.ResourceRunFailed(_)    => Red
      }
      .getOrElse(Blue)

  // If we are running, we want a circular spinning icon.
  // If we are completed, we want a checkmark.
  // Otherwise, no icon.
  private def determineIcon(op: Option[ResourceRunOperation]): Option[Icon] =
    op match {
      case Some(ResourceRunOperation.ResourceRunInFlight(_)) => RunningIcon.some
      case Some(ResourceRunOperation.ResourceRunCompleted(_)) =>
        CompletedIcon.some
      case Some(ResourceRunOperation.ResourceRunFailed(_)) => FailureIcon.some
      case _                                               => none
    }

  protected val component = ScalaComponent
    .builder[Props]("SubsystemControl")
    .render_P { p =>
      <.div(
        SeqexecStyles.notInMobile,
        p.resources.sorted.map { r =>
          val buttonIcon = determineIcon(p.resourcesCalls.get(r))
          val labeled: js.UndefOr[LabelPosition] = buttonIcon
            .as(LabelPosition.Left: js.UndefOr[LabelPosition])
            .getOrElse(js.undefined)

          Popup(
            trigger = Button(
              size  = Small,
              color = buttonColor(p.resourcesCalls.get(r)),
              disabled = p.resourcesCalls.get(r).exists {
                case ResourceRunOperation.ResourceRunInFlight(_) => true
                case _                                           => false
              },
              labelPosition = labeled,
              icon          = buttonIcon.isDefined,
              onClickE =
                if (p.canOperate)(requestResourceCall(p.id, p.stepId, r)) else js.undefined,
              clazz = SeqexecStyles.defaultCursor.unless_(p.canOperate)
            )(buttonIcon.whenDefined(identity), r.show)
          )(s"Configure ${r.show}")
        }.toTagMod
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
