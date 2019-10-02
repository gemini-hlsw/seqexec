// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.implicits._
import seqexec.web.client.model.StepItems.StepStateSnapshot
import web.client.ReactProps
import japgolly.scalajs.react.internal.CatsReactExt
import seqexec.model.enum.NodAndShuffleStage
import seqexec.web.client.components.{DividedProgress, SeqexecStyles}
import cats.implicits._
import seqexec.model.NodAndShuffleStep
import seqexec.model.operations._
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.reusability._

final case class NodAndShuffleCycleProgress(state: StepStateSnapshot) extends ReactProps {
  @inline def render: VdomElement = NodAndShuffleCycleProgress.component(this)
}

object NodAndShuffleCycleProgress extends CatsReactExt {
  type Props = NodAndShuffleCycleProgress

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("NodAndShuffleCycleProgress")
    .stateless
    .render_P { p =>
      val nsStatus = p.state.step.asInstanceOf[NodAndShuffleStep].nsStatus
      val isInError = !p.state.isNSRunning && p.state.isNSInError

      DividedProgress(
        List.range(1, nsStatus.cycles + 1).map(_.show),
        nsStatus.nodExposureTime.toSeconds.toInt * NodAndShuffleStage.NsSequence.length,
        value = 0,
        completeSectionColor = if (isInError) "red".some else "green".some,
        ongoingSectionColor = if (isInError) "red".some else "blue".some,
        progressCls = List(SeqexecStyles.observationProgressBar),
        barCls = List(SeqexecStyles.observationBar),
        labelCls = List(SeqexecStyles.observationLabel)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

final case class NodAndShuffleNodProgress(state: StepStateSnapshot) extends ReactProps {
  @inline def render: VdomElement = NodAndShuffleNodProgress.component(this)
}

object NodAndShuffleNodProgress extends CatsReactExt {
  type Props = NodAndShuffleNodProgress

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val nodSections: List[DividedProgress.Label] =
    NodAndShuffleStage.NsSequence.map(_.symbol.name).toList

  protected val component = ScalaComponent
    .builder[Props]("NodAndShuffleNodProgress")
    .stateless
    .render_P { p =>
      val nsStatus = p.state.step.asInstanceOf[NodAndShuffleStep].nsStatus
      val isInError = !p.state.isNSRunning && p.state.isNSInError

      DividedProgress(
        nodSections,
        sectionTotal = nsStatus.nodExposureTime.toSeconds.toInt,
        value = 0,
        completeSectionColor = if (isInError) "red".some else "green".some,
        ongoingSectionColor = if (isInError) "red".some else "blue".some,
        progressCls = List(SeqexecStyles.observationProgressBar),
        barCls = List(SeqexecStyles.observationBar),
        labelCls = List(SeqexecStyles.observationLabel)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

final case class NodAndShuffleCycleRowProps(
  clientStatus : ClientStatus,
  stateSnapshot: StepStateSnapshot
) extends ReactProps {
  @inline def render: VdomElement = NodAndShuffleCycleRow.component(this)
}

object NodAndShuffleCycleRow extends CatsReactExt {
  def apply(clientStatus : ClientStatus)(state: StepStateSnapshot): NodAndShuffleCycleRowProps =
    NodAndShuffleCycleRowProps(clientStatus, state)

  type Props = NodAndShuffleCycleRowProps

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val propsControlButtonResolver: ControlButtonResolver[Props] =
    ControlButtonResolver.build(p => (p.clientStatus, p.stateSnapshot.state, p.stateSnapshot.step))

  protected[steps] val component = ScalaComponent
    .builder[Props]("NodAndShuffleCycleRow")
    .stateless
    .render_P { p =>
      <.span(
        SeqexecStyles.nodAndShuffleDetailRow,
        NodAndShuffleCycleProgress(p.stateSnapshot),
        <.span(
          SeqexecStyles.nodAndShuffleControls,
          ControlButtons(
            p.stateSnapshot.obsId,
            p.stateSnapshot.instrument.operations[OperationLevel.NsCycle](p.stateSnapshot.step.isObservePaused),
            p.stateSnapshot.state,
            p.stateSnapshot.step.id,
            p.stateSnapshot.step.isObservePaused,
            p.stateSnapshot.tabOperations
          )
        ).when(p.controlButtonsActive)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

final case class NodAndShuffleNodRowProps(
  clientStatus : ClientStatus,
  stateSnapshot: StepStateSnapshot
) extends ReactProps {
  @inline def render: VdomElement = NodAndShuffleNodRow.component(this)
}

object NodAndShuffleNodRow extends CatsReactExt {
  def apply(clientStatus : ClientStatus)(state: StepStateSnapshot): NodAndShuffleNodRowProps =
    NodAndShuffleNodRowProps(clientStatus, state)

  type Props = NodAndShuffleNodRowProps

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val propsControlButtonResolver: ControlButtonResolver[Props] =
    ControlButtonResolver.build(p => (p.clientStatus, p.stateSnapshot.state, p.stateSnapshot.step))

  protected[steps] val component = ScalaComponent
    .builder[Props]("NodAndShuffleNodRow")
    .stateless
    .render_P { p =>
      <.span(
        SeqexecStyles.nodAndShuffleDetailRow,
        NodAndShuffleNodProgress(p.stateSnapshot),
        <.span(
          SeqexecStyles.nodAndShuffleControls,
          ControlButtons(
            p.stateSnapshot.obsId,
            p.stateSnapshot.instrument.operations[OperationLevel.NsNod](p.stateSnapshot.step.isObservePaused),
            p.stateSnapshot.state,
            p.stateSnapshot.step.id,
            p.stateSnapshot.step.isObservePaused,
            p.stateSnapshot.tabOperations
            )
        ).when(p.controlButtonsActive)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}