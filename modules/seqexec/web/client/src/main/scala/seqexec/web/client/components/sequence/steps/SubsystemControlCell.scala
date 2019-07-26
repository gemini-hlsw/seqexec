// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.ReactEvent
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.Reusability._
import gem.Observation
import react.common.implicits._
import scala.collection.immutable.SortedMap
import seqexec.model.enum._
import seqexec.model.StepId
import seqexec.web.client.actions.RequestResourceRun
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.elements.icon.Icon.IconCircleNotched
import seqexec.web.client.semanticui.elements.icon.Icon.IconCheckmark
import seqexec.web.client.semanticui.elements.icon.Icon.IconAttention
import seqexec.web.client.semanticui.Size
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui.elements.icon.Icon

/**
  * Contains the control buttons for each subsystem
  */
object SubsystemControlCell {
  final case class Props(
    id:             Observation.Id,
    stepId:         Int,
    resources:      List[Resource],
    resourcesCalls: SortedMap[Resource, ResourceRunOperation]
  )

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def requestResourceCall(
    id:     Observation.Id,
    stepId: StepId,
    r:      Resource
  )(e:      ReactEvent): Callback =
    e.preventDefaultCB *> e.stopPropagationCB *>
      SeqexecCircuit.dispatchCB(RequestResourceRun(id, stepId, r))

  private val CompletedIcon = IconCheckmark.copyIcon(
    fitted      = true,
    extraStyles = List(SeqexecStyles.completedIcon)
  )

  private val RunningIcon = IconCircleNotched.copyIcon(
    fitted      = true,
    loading     = true,
    extraStyles = List(SeqexecStyles.runningIcon)
  )

  private val FailureIcon = IconAttention.copyIcon(
    fitted      = true,
    inverted    = true,
    extraStyles = List(SeqexecStyles.errorIcon)
  )

  // We want blue if the resource operation is idle or does not exist: these are equivalent cases.
  private def buttonColor(op: Option[ResourceRunOperation]): Option[String] =
    op.map {
        case ResourceRunOperation.ResourceRunIdle         => "blue"
        case ResourceRunOperation.ResourceRunInFlight(_)  => "yellow"
        case ResourceRunOperation.ResourceRunCompleted(_) => "green"
        case ResourceRunOperation.ResourceRunFailed(_)    => "red"
      }
      .orElse("blue".some)

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

  private val component = ScalaComponent
    .builder[Props]("SubsystemControl")
    .render_P { p =>
      <.div(
        SeqexecStyles.notInMobile,
        p.resources.sorted.map { r =>
          val buttonIcon = determineIcon(p.resourcesCalls.get(r))
          Popup(
            Popup.Props("button", s"Configure ${r.show}"),
            Button(
              Button.Props(
                size  = Size.Small,
                color = buttonColor(p.resourcesCalls.get(r)),
                disabled = p.resourcesCalls.get(r).exists {
                  case ResourceRunOperation.ResourceRunInFlight(_) => true
                  case _                                           => false
                },
                labeled = buttonIcon
                  .as(Button.LeftLabeled)
                  .getOrElse(Button.NotLabeled),
                icon     = buttonIcon,
                onClickE = requestResourceCall(p.id, p.stepId, r) _
              ),
              r.show
            )
          )
        }.toTagMod
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
