// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import cats.data.Nested
import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import react.common._
import react.common.implicits._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ActionStatus
import seqexec.model.enum.Resource
import seqexec.model.enum.Instrument
import seqexec.model.{SequenceState, Step, StepId, StepState}
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
import seqexec.web.client.semanticui.elements.popup.Popup

/**
  * Component to display the step state and control
  */
final case class StepProgressCell(
  clientStatus : ClientStatus,
  stateSummary : StepStateSummary,
  selectedStep : Option[StepId],
  isPreview    : Boolean
) extends ReactProps {
  @inline def render: VdomElement = StepProgressCell.component(this)

  val step: Step = stateSummary.step
  val obsId: Observation.Id = stateSummary.obsId
  val instrument: Instrument = stateSummary.instrument
  val tabOperations: TabOperations = stateSummary.tabOperations
  val state: SequenceState = stateSummary.state

  val resourceRunRequested = tabOperations.resourceRunRequested

  def stepSelected(i: StepId): Boolean =
    selectedStep.exists(_ === i) && !isPreview &&
      (clientStatus.isLogged || tabOperations.resourceRunNotIdle(i))

  def isStopping: Boolean =
    tabOperations.stopRequested === StopOperation.StopInFlight
}

object StepProgressCell {
  type Props = StepProgressCell

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val propsControlButtonResolver: ControlButtonResolver[Props] =
    ControlButtonResolver.build(p => (p.clientStatus, p.state, p.step))

  def labelColor(status: ActionStatus): String = status match {
    case ActionStatus.Pending   => "gray"
    case ActionStatus.Running   => "yellow"
    case ActionStatus.Completed => "green"
    case ActionStatus.Paused    => "orange"
    case ActionStatus.Failed    => "red"
    case ActionStatus.Aborted   => "red"
  }

  def labelIcon(status: ActionStatus): Option[Icon] = status match {
    case ActionStatus.Pending   => None
    case ActionStatus.Running   => IconCircleNotched.copyIcon(loading = true).some
    case ActionStatus.Completed => IconCheckmark.some
    case ActionStatus.Paused    => IconPause.some
    case ActionStatus.Failed    => IconStopCircle.some
    case ActionStatus.Aborted   => IconStopCircle.some
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

  def stepControlButtons(props: Props): TagMod =
    StepsControlButtons(
      props.obsId,
      props.instrument,
      props.state,
      props.step.id,
      props.step.isObservePaused,
      props.step.isMultiLevel,
      props.tabOperations
    ).when(props.controlButtonsActive)

  def stepObservationStatusAndFile(
    props:   Props,
    fileId:  ImageFileId,
    paused:  Boolean
  ): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      if(props.stateSummary.isBias) {
        BiasStatus(
          props.obsId,
          props.step.id,
          fileId,
          stopping = !paused && props.isStopping,
          paused
        )
      } else {
        props.stateSummary.nsStatus.fold[VdomElement] {
          ObservationProgressBar(
            props.obsId,
            props.step.id,
            fileId,
            stopping = !paused && props.isStopping,
            paused)
        } { nsStatus =>
          NodAndShuffleProgressMessage(
            props.obsId,
            props.step.id,
            fileId,
            props.isStopping,
            paused,
            nsStatus)
        }
      },
      stepControlButtons(props)
    )

  def stepObservationPausing(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      <.div(
        SeqexecStyles.specialStateLabel,
        props.state.show
      ),
      stepControlButtons(props)
    )

  private def textWithPopup(text: String): VdomElement =
    Popup("span", text)(
      <.span(text)
    )

  def stepSubsystemControl(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      RunFromStep(
        props.obsId,
        props.step.id,
        props.tabOperations.resourceInFlight(props.step.id),
        props.tabOperations.startFromRequested
      ).when(props.step.canRunFrom && props.clientStatus.canOperate),
      <.div(
        SeqexecStyles.specialStateLabel,
        if (props.stateSummary.isAC) {
          if (props.stateSummary.isACRunning) {
            "Running Align & Calib..."
          } else if (props.stateSummary.anyError) {
            textWithPopup(props.step.show)
          } else {
            "Align & Calib"
          }
        } else {
          textWithPopup(props.step.show)
        }),
      SubsystemControlCell(
        props.obsId,
        props.step.id,
        Nested(Step.configStatus.getOption(props.step)).map(_._1).value.orEmpty,
        props.resourceRunRequested,
        props.clientStatus.canOperate)
      )

  def stepPaused(props: Props): VdomElement =
    <.div(
      SeqexecStyles.configuringRow,
      props.step.show
    )

  def stepDisplay(props: Props): VdomElement =
    (props.state, props.step) match {
      case (f, s) if s.status === StepState.Running && s.fileId.isEmpty && f.userStopRequested =>
        // Case pause at the sequence level
        stepObservationPausing(props)
      case (_, s) if s.status === StepState.Running && s.fileId.isEmpty                        =>
        // Case configuring, label and status icons
        stepSystemsStatus(s)
      case (_, s) if s.isObservePaused && s.fileId.isDefined                                   =>
        // Case for exposure paused, label and control buttons
        stepObservationStatusAndFile(props, s.fileId.orEmpty, paused = true)
      case (_, s) if s.status === StepState.Running && s.fileId.isDefined                      =>
        // Case for a exposure onging, progress bar and control buttons
        stepObservationStatusAndFile(props, s.fileId.orEmpty, paused = false)
      case (_, s) if s.wasSkipped                                                              =>
        <.p("Skipped")
      case (_, _) if props.step.skip                                                           =>
        <.p("Skip")
      case (_, s)
        if s.status === StepState.Completed && s.fileId.isDefined                              =>
        <.p(SeqexecStyles.componentLabel, s.fileId.orEmpty)
      case (_, s) if props.stepSelected(s.id) && s.canConfigure                                =>
        stepSubsystemControl(props)
      case _                                                                                   =>
        <.p(SeqexecStyles.componentLabel, props.step.show)
    }

  protected val component = ScalaComponent
    .builder[Props]("StepProgressCell")
    .stateless
    .render_P(stepDisplay)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
