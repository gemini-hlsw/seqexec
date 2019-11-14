// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import diode.react.ReactConnectProxy
import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.TimerSupport
import monocle.macros.Lenses
import react.common.implicits._
import react.common.Css
import seqexec.web.client.model.StepItems.StepStateSummary
import seqexec.model.enum.NodAndShuffleStage
import seqexec.web.client.components.{DividedProgress, SeqexecStyles}
import seqexec.model.dhs.ImageFileId
import seqexec.model.{NSObservationProgress, NodAndShuffleStatus, ObserveStage, StepId}
import seqexec.model.operations._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.{ClientStatus, StopOperation}
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui._

import scala.math.max
import web.client.ReactProps

final case class NodAndShuffleProgressMessage(
                                               obsId : Observation.Id,
                                               stepId: StepId,
                                               fileId: ImageFileId,
                                               stopping: Boolean,
                                               paused: Boolean,
                                               nsStatus: NodAndShuffleStatus
                                             ) extends ReactProps {
  @inline def render: VdomElement = NodAndShuffleProgressMessage.component(this)

  protected[steps] val connect =
    SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader[NSObservationProgress](obsId, stepId))
}

object NodAndShuffleProgressMessage extends ProgressLabel {
  type Props = NodAndShuffleProgressMessage

  @Lenses
  protected case class State(progressConnect: ReactConnectProxy[Option[NSObservationProgress]])

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val stateReuse: Reusability[State] = Reusability.always

  protected[steps] val component = ScalaComponent
    .builder[Props]("NodAndShuffleProgress")
    .initialStateFromProps(p => State(p.connect))
    .render_PS { (p, s) =>
      <.div(
        SeqexecStyles.specialStateLabel,
        SeqexecStyles.progressMessage,
        p.nsStatus.state.map[VdomElement] { nsState =>
          s.progressConnect { proxy =>
            val nodCount = NodAndShuffleStage.NsSequence.length
            val nodMillis = p.nsStatus.nodExposureTime.toMilliseconds.toInt
            val cycleMillis = nodMillis * nodCount
            val remainingCycles = p.nsStatus.cycles - nsState.sub.cycle - 1
            val remainingNods = nodCount - nsState.sub.stageIndex - 1
            val remainingNodMillis = proxy().foldMap(_.remaining.toMilliseconds.toInt)
            val remainingMillis = remainingCycles * cycleMillis + remainingNods * nodMillis + remainingNodMillis
            val stage = proxy().map(_.stage).getOrElse(ObserveStage.Idle)
            <.span(label(p.fileId, remainingMillis, p.stopping, p.paused, stage))
          }
        } getOrElse <.span(p.fileId)
        )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

final case class SmoothDividedProgressBar(
  sections            : List[DividedProgress.Label],
  sectionTotal        : DividedProgress.Quantity,
  value               : DividedProgress.Quantity,
  maxValue            : DividedProgress.Quantity,
  completeSectionColor: Option[String] = None,
  ongoingSectionColor : Option[String] = None,
  progressCls         : List[Css] = Nil,
  barCls              : List[Css],
  labelCls            : List[Css] = Nil,
  stopping            : Boolean,
  paused: Boolean
) extends SmoothProgressBarProps {
  @inline def render: VdomElement = SmoothDividedProgressBar.component(this)
}

object SmoothDividedProgressBar extends SmoothProgressBar[SmoothDividedProgressBar] {
  type Props = SmoothDividedProgressBar

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("SmoothDividedProgressBar")
    .initialStateFromProps(State.fromProps)
    .backend(x => new Backend(x))
    .render_PS { (p, s) =>
      DividedProgress(
        sections = p.sections,
        sectionTotal = p.sectionTotal,
        value = s.value,
        completeSectionColor = p.completeSectionColor,
        ongoingSectionColor = p.ongoingSectionColor,
        progressCls = p.progressCls,
        barCls = p.barCls,
        labelCls = p.labelCls
        )
    }
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps($ =>
                                 $.backend.newStateFromProps($.currentProps, $.nextProps))
    .configure(TimerSupport.install)
    .configure(Reusability.shouldComponentUpdate)
    .build
}

sealed trait NodAndShuffleProgressProps extends ReactProps {
  val summary: StepStateSummary

  def isStopping: Boolean =
    summary.tabOperations.stopRequested === StopOperation.StopInFlight

  protected[steps] val connect =
    SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader[NSObservationProgress](summary.obsId, summary.step.id))
}

sealed trait NodAndShuffleProgress {
  type Props <: NodAndShuffleProgressProps

  // From diode doc (Usage with React): "Having a single reference to (connect)
  // during your components lifecycle ensures that React will update your
  // component rather than unmounting and remounting it."
  @Lenses
  protected case class State(progressConnect: ReactConnectProxy[Option[NSObservationProgress]])

  implicit val propsReuse: Reusability[Props]
  implicit val stateReuse: Reusability[State] = Reusability.always

  protected def sections(nsStatus: NodAndShuffleStatus): List[DividedProgress.Label]

  protected def quantitiesFromNodMillis(
    cycleIndex: Int,
    nodIndex:   Int,
    nodMillis:  DividedProgress.Quantity
  ): (DividedProgress.Quantity, DividedProgress.Quantity) // (sectionTotal, currentValue)

  protected[steps] val component = ScalaComponent
    .builder[Props]("NodAndShuffleProgress")
    .initialStateFromProps(p => State(p.connect))
    .render_PS { (p, s) =>
      s.progressConnect { proxy =>
        val (totalMillis, remainingMillis, cycleIndex, nodIndex) =
          proxy().foldMap(p =>
            (p.total.toMilliseconds.toInt, p.remaining.toMilliseconds.toInt, p.sub.cycle.toInt, p.sub.stageIndex))
        val elapsedMillis = totalMillis - max(0, remainingMillis)

        p.summary.nsStatus.map[VdomElement] { nsStatus =>
          val isInError = !p.summary.isNSRunning && p.summary.isNSInError
          val nodMillis = nsStatus.nodExposureTime.toMilliseconds.toInt
          val (sectionTotal, currentValue) = quantitiesFromNodMillis(cycleIndex, nodIndex, nodMillis)

          SmoothDividedProgressBar(
            sections = sections(nsStatus),
            sectionTotal = sectionTotal,
            value = nsStatus.state.foldMap(_ => currentValue + elapsedMillis), // Only advance smooth bar if actually started
            maxValue = nsStatus.state.foldMap(_ => currentValue + nodMillis), // Only advance smooth bar if actually started
            completeSectionColor = if (isInError) "red".some else "green".some,
            ongoingSectionColor = if (isInError) "red".some else "blue".some,
            progressCls = List(SeqexecStyles.observationProgressBar),
            barCls = List(SeqexecStyles.observationBar),
            labelCls = List(SeqexecStyles.observationLabel),
            stopping = p.isStopping,
            paused = p.summary.step.isObservePaused
            )
        } getOrElse
          <.div("NodAndShuffleProgress invoked without a Nod&Shuffle step summary")
      }
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

final case class NodAndShuffleCycleProgress(summary: StepStateSummary)
  extends NodAndShuffleProgressProps {
    @inline def render: VdomElement = NodAndShuffleCycleProgress.component(this)
}

object NodAndShuffleCycleProgress extends NodAndShuffleProgress {
  type Props = NodAndShuffleCycleProgress

  implicit lazy val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected def sections(nsStatus: NodAndShuffleStatus): List[DividedProgress.Label] =
    List.range(1, nsStatus.cycles + 1).map(_.show)

  protected def quantitiesFromNodMillis(
    cycleIndex: Int,
    nodIndex:   Int,
    nodMillis: DividedProgress.Quantity
  ): (DividedProgress.Quantity, DividedProgress.Quantity) = {
    val cycleMillis = nodMillis * NodAndShuffleStage.NsSequence.length
    val currentValue = cycleIndex * cycleMillis + nodIndex * nodMillis
    (cycleMillis, currentValue)
  }
}

final case class NodAndShuffleNodProgress(summary: StepStateSummary)
  extends NodAndShuffleProgressProps {
    @inline def render: VdomElement = NodAndShuffleNodProgress.component(this)
}

object NodAndShuffleNodProgress extends NodAndShuffleProgress {
  type Props = NodAndShuffleNodProgress

  implicit lazy val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected def sections(nsStatus: NodAndShuffleStatus): List[DividedProgress.Label] =
    NodAndShuffleStage.NsSequence.map(_.symbol.name).toList

  protected def quantitiesFromNodMillis(
    cycleIndex: Int,
    nodIndex:   Int,
    nodMillis: DividedProgress.Quantity
  ): (DividedProgress.Quantity, DividedProgress.Quantity) = {
    val currentValue = nodIndex * nodMillis
    (nodMillis, currentValue)
  }
}

sealed trait NodAndShuffleRowProps extends ReactProps {
  val clientStatus: ClientStatus
  val stateSummary: StepStateSummary
}

sealed trait NodAndShuffleRow[L <: OperationLevel] {
  type Props <: NodAndShuffleRowProps

  implicit val propsReuse: Reusability[Props]

  implicit val propsControlButtonResolver: ControlButtonResolver[Props] =
    ControlButtonResolver.build(p => (p.clientStatus, p.stateSummary.state, p.stateSummary.step))

  implicit protected val operationLevelType: OperationLevelType[L]

  protected def progressControl(summary: StepStateSummary): VdomElement

  protected[steps] val component = ScalaComponent
    .builder[Props]("NodAndShuffleCycleRow")
    .stateless
    .render_P { p =>
      <.span(
        SeqexecStyles.nodAndShuffleDetailRow,
        progressControl(p.stateSummary),
        <.span(
          SeqexecStyles.nodAndShuffleControls,
          ControlButtons(
            p.stateSummary.obsId,
            p.stateSummary.instrument.operations[L](p.stateSummary.step.isObservePaused),
            p.stateSummary.state,
            p.stateSummary.step.id,
            p.stateSummary.step.isObservePaused,
            p.stateSummary.tabOperations,
            p.stateSummary.nsPendingObserveCmd
            )
          ).when(p.controlButtonsActive)
        )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

final case class NodAndShuffleCycleRowProps(
  clientStatus: ClientStatus,
  stateSummary: StepStateSummary
) extends NodAndShuffleRowProps {
  @inline def render: VdomElement = NodAndShuffleCycleRow.component(this)
}

object NodAndShuffleCycleRow extends NodAndShuffleRow[OperationLevel.NsCycle] {
  def apply(clientStatus: ClientStatus)(state: StepStateSummary): NodAndShuffleCycleRowProps =
    NodAndShuffleCycleRowProps(clientStatus, state)

  type Props = NodAndShuffleCycleRowProps

  implicit lazy val propsReuse: Reusability[Props] = Reusability.derive[Props]

  implicit protected val operationLevelType: OperationLevelType[OperationLevel.NsCycle] =
    implicitly[OperationLevelType[OperationLevel.NsCycle]]

  protected def progressControl(summary: StepStateSummary): VdomElement =
    NodAndShuffleCycleProgress(summary)
}

final case class NodAndShuffleNodRowProps(
  clientStatus: ClientStatus,
  stateSummary: StepStateSummary
) extends NodAndShuffleRowProps {
  @inline def render: VdomElement = NodAndShuffleNodRow.component(this)
}

object NodAndShuffleNodRow extends NodAndShuffleRow[OperationLevel.NsNod] {
  def apply(clientStatus: ClientStatus)(state: StepStateSummary): NodAndShuffleNodRowProps =
    NodAndShuffleNodRowProps(clientStatus, state)

  type Props = NodAndShuffleNodRowProps

  implicit lazy val propsReuse: Reusability[Props] = Reusability.derive[Props]

  implicit protected val operationLevelType: OperationLevelType[OperationLevel.NsNod] =
    implicitly[OperationLevelType[OperationLevel.NsNod]]

  protected def progressControl(summary: StepStateSummary): VdomElement =
    NodAndShuffleNodProgress(summary)
}
