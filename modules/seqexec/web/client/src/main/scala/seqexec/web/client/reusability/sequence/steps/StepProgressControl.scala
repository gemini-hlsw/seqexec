// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.{ ActionStatus, Resource }
import seqexec.model.StepState
import seqexec.model.{ StandardStep, Step }
import seqexec.web.client.circuit.StepsTableFocus
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.ModelOps._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.icon.Icon
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Component to display the step state and control
  */
object StepProgressCell {
  final case class Props(clientStatus: ClientStatus, focus: StepsTableFocus, step: Step) {
    val steps: List[Step] = focus.steps
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

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
    Label(Label.Props(s"${system.show}", color = labelColor(status).some, icon = labelIcon(status)))

  def stepSystemsStatus(step: StandardStep): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      <.div(
        SeqexecStyles.specialStateLabel,
        "Configuring"
      ),
      <.div(
        SeqexecStyles.subsystems,
        step.configStatus.map(Function.tupled(statusLabel)).toTagMod
      )
    )

  def controlButtonsActive(props: Props): Boolean =
    props.clientStatus.isLogged && props.focus.state.isRunning && (props.step.isObserving || props.step.isObservePaused || props.focus.state.userStopRequested)

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
        props.step.show
      ),
      StepsControlButtons(props.focus.id, props.focus.instrument, props.focus.state, props.step).when(controlButtonsActive(props))
    )

  def stepObservationPausing(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      <.div(
        SeqexecStyles.specialStateLabel,
        props.focus.state.show
      ),
      StepsControlButtons(props.focus.id, props.focus.instrument, props.focus.state, props.step).when(controlButtonsActive(props))
    )

  def stepPaused(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      props.step.show
    )

  def stepDisplay(props: Props): VdomElement =
    (props.focus.state, props.step) match {
      case (f, StandardStep(_, _, StepState.Running, _, _, _, _, _)) if f.userStopRequested =>
        // Case pause at the sequence level
        stepObservationPausing(props)
      case (_, s @ StandardStep(_, _, StepState.Running, _, _, None, _, _))                 =>
        // Case configuring, label and status icons
        stepSystemsStatus(s)
      case (_, s) if s.isObservePaused                                                      =>
        // Case for exposure paused, label and control buttons
        stepObservationStatus(props)
      case (_, StandardStep(_, _, StepState.Running, _, _, Some(fileId), _, _))             =>
        // Case for a exposure onging, progress bar and control buttons
        stepObservationStatusAndFile(props, fileId)
      case (_, s) if s.wasSkipped                                                           =>
        <.p("Skipped")
      case (_, _) if props.step.skip                                                        =>
        <.p("Skip")
      case (_, StandardStep(_, _, StepState.Completed, _, _, Some(fileId), _, _))             =>
        <.p(SeqexecStyles.componentLabel, fileId)
      case (_, _)                                                                           =>
        <.p(SeqexecStyles.componentLabel, props.step.show)
    }

  private val component = ScalaComponent.builder[Props]("StepProgressCell")
    .stateless
    .render_P(stepDisplay)
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}
