// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import scala.collection.immutable.SortedMap
import scala.scalajs.js.timers._

import cats._
import cats.syntax.all._
import lucuma.core.enums.Site
import monocle.Getter
import monocle.Lens
import monocle.Traversal
import monocle.function.At.atSortedMap
import monocle.function.Each.mapEach
import monocle.function.FilterIndex.sortedMapFilterIndex
import monocle.macros.Lenses
import seqexec.model.CalibrationQueueId
import seqexec.model.ClientId
import seqexec.model.Conditions
import seqexec.model.ExecutionQueueView
import seqexec.model.M1GuideConfig._
import seqexec.model.M2GuideConfig._
import seqexec.model.Observation
import seqexec.model.QueueId
import seqexec.model.SequenceView
import seqexec.model.SequencesQueue
import seqexec.model.TelescopeGuideConfig
import seqexec.model.enum.MountGuideOption._
import seqexec.web.client.components.SessionQueueTable
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.sequence.steps.StepsTable
import seqexec.web.client.circuit.UserLoginFocus
import seqexec.web.client.circuit.SequencesQueueFocus
import web.client.table._

/**
 * Root of the UI Model of the application
 */
@Lenses
final case class SeqexecAppRootModel(
  sequences:     SequencesQueue[SequenceView],
  ws:            WebSocketConnection,
  site:          Option[Site],
  clientId:      Option[ClientId],
  uiModel:       SeqexecUIModel,
  serverVersion: Option[String],
  guideConfig:   TelescopeGuideConfig,
  alignAndCalib: AlignAndCalibStep,
  pingInterval:  Option[SetTimeoutHandle]
)

object SeqexecAppRootModel {
  val NoSequencesLoaded: SequencesQueue[SequenceView] =
    SequencesQueue[SequenceView](Map.empty, Conditions.Default, none, SortedMap.empty, Nil)

  val Initial: SeqexecAppRootModel = SeqexecAppRootModel(
    NoSequencesLoaded,
    WebSocketConnection.Empty,
    none,
    none,
    SeqexecUIModel.Initial,
    none,
    TelescopeGuideConfig(MountGuideOff, M1GuideOff, M2GuideOff),
    AlignAndCalibStep.NoAction,
    None
  )

  val logDisplayL: Lens[SeqexecAppRootModel, SectionVisibilityState] =
    SeqexecAppRootModel.uiModel.andThen(SeqexecUIModel.globalLog).andThen(GlobalLog.display)

  val userLoginFocus: Lens[SeqexecAppRootModel, UserLoginFocus] =
    SeqexecAppRootModel.uiModel.andThen(SeqexecUIModel.userLoginFocus)

  val sessionQueueFilterL: Lens[SeqexecAppRootModel, SessionQueueFilter] =
    SeqexecAppRootModel.uiModel.andThen(SeqexecUIModel.sessionQueueFilter)

  val sequencesOnDisplayL: Lens[SeqexecAppRootModel, SequencesOnDisplay] =
    SeqexecAppRootModel.uiModel.andThen(SeqexecUIModel.sequencesOnDisplay)

  val sequenceTabsT: Traversal[SeqexecAppRootModel, SequenceTab] =
    SeqexecAppRootModel.sequencesOnDisplayL.andThen(SequencesOnDisplay.sequenceTabs)

  val sessionQueueL: Lens[SeqexecAppRootModel, List[SequenceView]] =
    SeqexecAppRootModel.sequences.andThen(SequencesQueue.sessionQueue)

  val sessionQueueTableStateL
    : Lens[SeqexecAppRootModel, TableState[SessionQueueTable.TableColumn]] =
    SeqexecAppRootModel.uiModel
      .andThen(SeqexecUIModel.appTableStates)
      .andThen(AppTableStates.sessionQueueTable)

  def stepsTableStateL(
    id: Observation.Id
  ): Lens[SeqexecAppRootModel, Option[TableState[StepsTable.TableColumn]]] =
    SeqexecAppRootModel.uiModel
      .andThen(SeqexecUIModel.appTableStates)
      .andThen(
        AppTableStates
          .stepsTableAtL(id)
      )

  val unsafeSequencesQueueFocus: Lens[SeqexecAppRootModel, SequencesQueueFocus] =
    Lens[SeqexecAppRootModel, SequencesQueueFocus](m =>
      SequencesQueueFocus(m.sequences,
                          m.uiModel.user.flatMap(u => m.uiModel.displayNames.get(u.username))
      )
    )(n => a => a.copy(sequences = n.sequences))

  val soundSettingL: Lens[SeqexecAppRootModel, SoundSelection] =
    SeqexecAppRootModel.uiModel.andThen(SeqexecUIModel.sound)

  val configTableStateL: Lens[SeqexecAppRootModel, TableState[StepConfigTable.TableColumn]] =
    SeqexecAppRootModel.uiModel
      .andThen(SeqexecUIModel.appTableStates)
      .andThen(AppTableStates.stepConfigTable)

  def executionQueuesT(
    id: QueueId
  ): Traversal[SeqexecAppRootModel, ExecutionQueueView] =
    SeqexecAppRootModel.sequences
      .andThen(SequencesQueue.queues[SequenceView])
      .andThen(
        sortedMapFilterIndex[QueueId, ExecutionQueueView].filterIndex((qid: QueueId) => qid === id)
      )

  val queuesT: Traversal[SeqexecAppRootModel, ExecutionQueueView] =
    SeqexecAppRootModel.sequences
      .andThen(SequencesQueue.queues[SequenceView])
      .andThen(mapEach[QueueId, ExecutionQueueView].each)

  val dayCalG: Getter[SeqexecAppRootModel, Option[ExecutionQueueView]] =
    SeqexecAppRootModel.sequences
      .andThen(SequencesQueue.queues[SequenceView])
      .andThen(atSortedMap[QueueId, ExecutionQueueView].at(CalibrationQueueId))
      .asGetter

  implicit val eq: Eq[SeqexecAppRootModel] =
    Eq.by(x => (x.sequences, x.ws, x.site, x.clientId, x.uiModel, x.serverVersion))
}
