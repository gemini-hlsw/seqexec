// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import gem.Observation
import seqexec.model.Step
import seqexec.model.StepState
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.semanticui.elements.icon.Icon
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.services.HtmlConstants.iconEmpty
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Component to display an icon for the state
  */
object StepToolsCell {
  final case class Props(clientStatus:       ClientStatus,
                         step:               Step,
                         rowHeight:          Int,
                         isPreview:          Boolean,
                         nextStepToRun:      Option[Int],
                         obsId:              Observation.Id,
                         firstRunnableIndex: Int,
                         breakPointEnterCB:  Int => Callback,
                         breakPointLeaveCB:  Int => Callback,
                         heightChangeCB:     Int => Callback)

  implicit val propsReuse: Reusability[Props] =
    Reusability.caseClassExcept[Props]('heightChangeCB,
                                       'breakPointEnterCB,
                                       'breakPointLeaveCB)

  private val component = ScalaComponent
    .builder[Props]("StepToolsCell")
    .stateless
    .render_P { p =>
      <.div(
        SeqexecStyles.controlCell,
        StepBreakStopCell(
          StepBreakStopCell.Props(p.clientStatus,
                                  p.step,
                                  p.rowHeight,
                                  p.obsId,
                                  p.firstRunnableIndex,
                                  p.breakPointEnterCB,
                                  p.breakPointLeaveCB,
                                  p.heightChangeCB))
          .when(p.clientStatus.isLogged)
          .unless(p.isPreview),
        StepIconCell(
          StepIconCell.Props(p.step.status,
                             p.step.skip,
                             p.nextStepToRun.forall(_ === p.step.id)))
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
  * Component to display an icon for the state
  */
object StepIconCell {
  final case class Props(status: StepState, skip: Boolean, nextToRun: Boolean)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def stepIcon(p: Props): VdomNode =
    p.status match {
      case StepState.Completed => IconCheckmark
      case StepState.Running   => IconCircleNotched.copyIcon(loading = true)
      case StepState.Failed(_) => IconAttention
      case StepState.Skipped =>
        IconReply.copyIcon(fitted  = true,
                           rotated = Icon.Rotated.CounterClockwise)
      case _ if p.skip =>
        IconReply.copyIcon(fitted  = true,
                           rotated = Icon.Rotated.CounterClockwise)
      case _ if p.nextToRun => IconChevronRight
      case _                => iconEmpty
    }

  private def stepStyle(p: Props): GStyle =
    p.status match {
      case StepState.Running   => SeqexecStyles.runningIconCell
      case StepState.Skipped   => SeqexecStyles.skippedIconCell
      case StepState.Completed => SeqexecStyles.completedIconCell
      case StepState.Failed(_) => SeqexecStyles.errorCell
      case _ if p.skip         => SeqexecStyles.skippedIconCell
      case _                   => SeqexecStyles.iconCell
    }

  private val component = ScalaComponent
    .builder[Props]("StepIconCell")
    .stateless
    .render_P { p =>
      <.div(
        stepStyle(p),
        // stepIcon(p)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
