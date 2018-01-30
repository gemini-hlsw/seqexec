// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model.{ActionStatus, Resource, StandardStep, Step, StepState}
import edu.gemini.seqexec.web.client.circuit.{ClientStatus, StepsTableFocus}
import edu.gemini.seqexec.web.client.ModelOps._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import scalacss.ScalaCssReact._

import scalaz.syntax.show._
import scalaz.syntax.std.option._

/**
  * Component to display the step state and control
  */
object StepProgressCell {
  final case class Props(clientStatus: ClientStatus, focus: StepsTableFocus, step: Step) {
    val steps: List[Step] = focus.steps
  }

  def labelColor(status: ActionStatus): String = status match {
    case ActionStatus.Pending   => "gray"
    case ActionStatus.Running   => "yellow"
    case ActionStatus.Completed => "green"
    case ActionStatus.Paused    => "orange"
    case ActionStatus.Failed    => "red"
  }

  def labelIcon(status: ActionStatus): Option[Icon] = status match {
    case ActionStatus.Pending   => None
    case ActionStatus.Running   => IconCircleNotched.copyIcon(loading = true).some
    case ActionStatus.Completed => IconCheckmark.some
    case ActionStatus.Paused    => IconPause.some
    case ActionStatus.Failed    => IconStopCircle.some
  }

  def statusLabel(system: Resource, status: ActionStatus): VdomNode =
    Label(Label.Props(s"${system.shows}", color = labelColor(status).some, icon = labelIcon(status)))

  def stepProgress(state: SequenceState, step: Step): VdomNode =
    (state, step.status) match {
      case (s, StepState.Running) if s.userStopRequested =>
        <.div(state.shows)
      case (s, _) if SequenceState.internalStopRequested(s) =>
        <.div(step.status.shows)
      case (_, StepState.Pending) =>
        step.fileId.fold(<.div("Pending"))(_ => <.div("Configuring"))
      case (_, StepState.Running) =>
        step.fileId.fold(<.div(stepSystemsStatus(step)): VdomNode)(fileId => ObservationProgressBar(fileId): VdomNode)
      case (_, StepState.Completed) =>
        step.fileId.getOrElse(""): String
      case _ =>
        step.file.getOrElse(""): String
    }

  def stepSystemsStatus(step: Step): VdomElement =
    step match {
      case StandardStep(_, _, _, _, _, _, configStatus, _) =>
        <.div(
          SeqexecStyles.configuringRow,
          <.div(
            "Configuring"
          ),
          <.div(
            SeqexecStyles.subsystems,
            configStatus.map(Function.tupled(statusLabel)).toTagMod
          )
        )
      case _ =>
        <.div(step.status.shows)
    }

  def stepDisplay(focus: StepsTableFocus, step: Step): VdomElement =
    (focus.state, step.status) match {
      case (_, StepState.Running) => stepSystemsStatus(step)
      // case (s, StepState.Running | StepState.Paused)     => controlButtons(status.isLogged, p, step)
      // case (_, StepState.Failed(msg))                    => stepInError(status.isLogged, isPartiallyExecuted(p), msg)
      case (_, _) if step.skip    => <.p("Skipped")
      case (_, _)                 => <.p(step.status.shows)
    }

  private val component = ScalaComponent.builder[Props]("StepProgressCell")
    .stateless
    .render_P { p =>
      stepDisplay(p.focus, p.step)
    }.build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}
