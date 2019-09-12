// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import cats.data.Nested
import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import react.common.implicits._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ActionStatus
import seqexec.model.enum.Resource
import seqexec.model.enum.Instrument
import seqexec.model.StepState
import seqexec.model.Step
import seqexec.model.StepId
import seqexec.model.SequenceState
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.model.TabOperations
import seqexec.web.client.model.StopOperation
import seqexec.web.client.model.StepItems._
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.icon.Icon
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.reusability._

/**
  * Component to display the step state and control
  */
object StepProgressCell {

  final case class Props(clientStatus: ClientStatus,
                         instrument   : Instrument,
                         obsId        : Observation.Id,
                         state        : SequenceState,
                         step         : Step,
                         selectedStep : Option[StepId],
                         isPreview    : Boolean,
                         tabOperations: TabOperations) {

    val resourceRunRequested = tabOperations.resourceRunRequested

    def stepSelected(i: StepId): Boolean =
      selectedStep.exists(_ === i) && !isPreview &&
        (clientStatus.isLogged || tabOperations.resourceRunNotIdle(i))

    def isStopping: Boolean =
      tabOperations.stopRequested === StopOperation.StopInFlight

    val stateSnapshot: StepStateSnapshot =
      StepStateSnapshot(step, instrument, tabOperations, state)
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
    Label(
      Label.Props(s"${system.show}",
                  color = labelColor(status).some,
                  icon  = labelIcon(status)))

  def stepSystemsStatus(step: Step): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      <.div(
        SeqexecStyles.specialStateLabel,
        "Configuring"
        ),
      <.div(
        SeqexecStyles.subsystems,
        Step.configStatus.getOption(step)
          .orEmpty
          .sortBy(_._1)
          .map(Function.tupled(statusLabel))
          .toTagMod
      )
    )

  def controlButtonsActive(props: Props): Boolean =
    props.clientStatus.isLogged && props.state.isRunning &&
      (props.step.isObserving || props.step.isObservePaused || props.state.userStopRequested)

  def stepObservationStatusAndFile(
    props:  Props,
    stepId: StepId,
    fileId: ImageFileId
  ): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      ObservationProgressBar(
        ObservationProgressBar
          .Props(props.obsId,
                 stepId,
                 fileId,
                 stopping = props.isStopping,
                 paused   = false)),
      StepsControlButtons(
        StepsControlButtons.Props(props.obsId,
                                  props.instrument,
                                  props.state,
                                  props.step.id,
                                  props.step.isObservePaused,
                                  props.tabOperations))
        .when(controlButtonsActive(props))
    )

  def stepObservationPaused(props:  Props,
                            stepId: StepId,
                            fileId: ImageFileId): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      ObservationProgressBar(
        ObservationProgressBar
          .Props(props.obsId, stepId, fileId, stopping = false, paused = true)),
      StepsControlButtons(
        StepsControlButtons.Props(props.obsId,
                                  props.instrument,
                                  props.state,
                                  props.step.id,
                                  props.step.isObservePaused,
                                  props.tabOperations))
        .when(controlButtonsActive(props))
      )

  def stepObservationPausing(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      <.div(
        SeqexecStyles.specialStateLabel,
        props.state.show
        ),
      StepsControlButtons(
        StepsControlButtons.Props(props.obsId,
                                  props.instrument,
                                  props.state,
                                  props.step.id,
                                  props.step.isObservePaused,
                                  props.tabOperations))
        .when(controlButtonsActive(props))
      )

  def stepSubsystemControl(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      RunFromStep(
        RunFromStep.Props(props.obsId,
                          props.step.id,
                          props.tabOperations.resourceInFlight(props.step.id),
                          props.tabOperations.startFromRequested))
        .when(props.step.canRunFrom && props.clientStatus.canOperate),
      <.div(
        SeqexecStyles.specialStateLabel,
        if (props.stateSnapshot.isAC) {//} && !props.stateSnapshot.anyError) {
          if (props.stateSnapshot.isACRunning) {
            "Running Align & Calib..."
          } else if (props.stateSnapshot.anyError) {
            props.step.show
          } else {
            "Align & Calib"
          }
        } else {
          props.step.show
        }),
      SubsystemControlCell(
        SubsystemControlCell
          .Props(
            props.obsId,
            props.step.id,
            Nested(Step.configStatus.getOption(props.step)).map(_._1).value.orEmpty,
            props.resourceRunRequested,
            props.clientStatus.canOperate))
    )

  def stepPaused(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      props.step.show
    )

  def stepDisplay(props: Props): VdomElement = {
    (props.state, props.step) match {
      case (f, s) if s.status === StepState.Running && f.userStopRequested =>
        // Case pause at the sequence level
        stepObservationPausing(props)
      case (_, s) if s.status === StepState.Running && s.fileId.isEmpty    =>
        // Case configuring, label and status icons
        stepSystemsStatus(s)
      case (_, s) if s.isObservePaused && s.fileId.isDefined               =>
        // Case for exposure paused, label and control buttons
        stepObservationPaused(props, s.id, s.fileId.orEmpty)
      case (_, s) if s.status === StepState.Running && s.fileId.isDefined  =>
        // Case for a exposure onging, progress bar and control buttons
        stepObservationStatusAndFile(props, s.id, s.fileId.orEmpty)
      case (_, s) if s.wasSkipped                                          =>
        <.p("Skipped")
      case (_, _) if props.step.skip                                       =>
        <.p("Skip")
      case (_, s)
        if s.status === StepState.Completed && s.fileId.isDefined          =>
        <.p(SeqexecStyles.componentLabel, s.fileId.orEmpty)
      case (_, s) if props.stepSelected(s.id) && s.canConfigure            =>
        stepSubsystemControl(props)
      case _                                                               =>
        <.p(SeqexecStyles.componentLabel, props.step.show)
    }
  }

  private val component = ScalaComponent
    .builder[Props]("StepProgressCell")
    .stateless
    .render_P(stepDisplay)
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}
