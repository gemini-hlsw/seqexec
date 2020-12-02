// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.syntax.all._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.elements.icon.IconRotated
import seqexec.model.Observation
import seqexec.model.Step
import seqexec.model.StepState
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.reusability._
import seqexec.web.client.services.HtmlConstants.iconEmpty

/**
  * Component to display an icon for the state
  */
final case class StepToolsCell(
  clientStatus:       ClientStatus,
  step:               Step,
  rowHeight:          Int,
  secondRowHeight:    Int,
  isPreview:          Boolean,
  nextStepToRun:      Option[Int],
  obsId:              Observation.Id,
  firstRunnableIndex: Int,
  breakPointEnterCB:  Int => Callback,
  breakPointLeaveCB:  Int => Callback,
  heightChangeCB:     Int => Callback
) extends ReactProps[StepToolsCell](StepToolsCell.component)

object StepToolsCell {
  type Props = StepToolsCell

  implicit val propsReuse: Reusability[Props] =
    Reusability.caseClassExcept[Props]("heightChangeCB",
                                       "breakPointEnterCB",
                                       "breakPointLeaveCB")

  protected val component = ScalaComponent
    .builder[Props]("StepToolsCell")
    .stateless
    .render_P { p =>
      <.div(
        SeqexecStyles.controlCell,
        StepBreakStopCell(
          p.clientStatus,
          p.step,
          p.rowHeight,
          p.obsId,
          p.firstRunnableIndex,
          p.breakPointEnterCB,
          p.breakPointLeaveCB,
          p.heightChangeCB
        ).when(p.clientStatus.isLogged)
          .unless(p.isPreview),
        StepIconCell(
          p.step.status,
          p.step.skip,
          p.nextStepToRun.forall(_ === p.step.id),
          p.rowHeight - p.secondRowHeight
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

/**
  * Component to display an icon for the state
  */
final case class StepIconCell(
  status:    StepState,
  skip:      Boolean,
  nextToRun: Boolean,
  height:    Int
) extends ReactProps[StepIconCell](StepIconCell.component)

object StepIconCell {
  type Props = StepIconCell

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private def stepIcon(p: Props): VdomNode =
    p.status match {
      case StepState.Completed => IconCheckmark
      case StepState.Running   => IconCircleNotched.loading(true)
      case StepState.Failed(_) => IconAttention
      case StepState.Skipped =>
        IconReply.copy(fitted = true, rotated = IconRotated.CounterClockwise)
      case _ if p.skip =>
        IconReply.copy(fitted = true, rotated = IconRotated.CounterClockwise)
      case _ if p.nextToRun => IconChevronRight
      case _                => iconEmpty
    }

  private def stepStyle(p: Props): Css =
    p.status match {
      case StepState.Running   => SeqexecStyles.runningIconCell
      case StepState.Skipped   => SeqexecStyles.skippedIconCell
      case StepState.Completed => SeqexecStyles.completedIconCell
      case StepState.Failed(_) => SeqexecStyles.errorCell
      case _ if p.skip         => SeqexecStyles.skippedIconCell
      case _                   => SeqexecStyles.iconCell
    }

  protected val component = ScalaComponent
    .builder[Props]("StepIconCell")
    .stateless
    .render_P(p =>
      <.div(
        ^.height := p.height.px,
        stepStyle(p),
        stepIcon(p)
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build
}
