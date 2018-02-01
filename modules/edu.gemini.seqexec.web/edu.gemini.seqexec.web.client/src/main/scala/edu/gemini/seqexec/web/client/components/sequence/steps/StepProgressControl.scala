// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.dhs.ImageFileId
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

  /*def stepProgress(state: SequenceState, step: Step): VdomNode =
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
    }*/

  def stepSystemsStatus(step: StandardStep): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      <.div(
        "Configuring"
      ),
      <.div(
        SeqexecStyles.subsystems,
        step.configStatus.map(Function.tupled(statusLabel)).toTagMod
      )
    )

  def controlButtonsActive(props: Props): Boolean =
    props.clientStatus.isLogged && props.focus.state.isRunning && (props.step.isObserving || props.step.isObservePaused)

  def stepObservationStatusAndFile(props: Props, fileId: ImageFileId): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      ObservationProgressBar(fileId),
      StepsControlButtons(props.focus.id, props.focus.instrument, props.focus.state, props.step).when(controlButtonsActive(props))
    )

  def stepObservationStatus(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      <.div(
        SeqexecStyles.specialStateLabel,
        props.step.shows
      ),
      StepsControlButtons(props.focus.id, props.focus.instrument, props.focus.state, props.step).when(controlButtonsActive(props))
    )

  def stepPaused(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      props.step.shows
    )

  def stepDisplay(props: Props): VdomElement =
    (props.focus.state, props.step) match {
      case (f, StandardStep(_, _, s @ StepState.Running, _, _, _, _, _)) if f.userStopRequested =>
        // Case pause at the sequence level
        stepObservationStatus(props)
      case (_, s @ StandardStep(_, _, StepState.Running, _, _, None, _, _))         =>
        // Case configuring, label and status icons
        stepSystemsStatus(s)
      case (f, s) if s.isObservePaused =>
        // Case for exposure paused, label and control buttons
        stepObservationStatus(props)
      case (f, StandardStep(_, _, StepState.Running, _, _, Some(fileId), _, _)) =>
        // Case for a exposure onging, progress bar and control buttons
        stepObservationStatusAndFile(props, fileId)
      // case (s, StepState.Running | StepState.Paused)     => controlButtons(status.isLogged, p, step)
      // case (_, StepState.Failed(msg))                    => stepInError(status.isLogged, isPartiallyExecuted(p), msg)
      case (_, _) if props.step.skip    => <.p("Skipped")
      case (_, _)                 => <.p(props.step.shows)
    }

  private val component = ScalaComponent.builder[Props]("StepProgressCell")
    .stateless
    .render_P { p =>
      stepDisplay(p)
    }.build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}
