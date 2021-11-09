// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import scala.collection.immutable.SortedMap
import scala.scalajs.js.timers._

import cats._
import cats.syntax.all._
import lucuma.core.enum.Site
import monocle.Getter
import monocle.Lens
import monocle.Traversal
import monocle.function.At.at
import monocle.function.At.atSortedMap
import monocle.function.Each.each
import monocle.function.FilterIndex.filterIndex
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
import seqexec.web.client.handlers.UserLoginFocus
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
    SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.globalLog ^|->
      GlobalLog.display

  val userLoginFocus: Lens[SeqexecAppRootModel, UserLoginFocus] =
    SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.userLoginFocus

  val sessionQueueFilterL: Lens[SeqexecAppRootModel, SessionQueueFilter] =
    SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.sessionQueueFilter

  val sequencesOnDisplayL: Lens[SeqexecAppRootModel, SequencesOnDisplay] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.sequencesOnDisplay

  val sequenceTabsT: Traversal[SeqexecAppRootModel, SequenceTab] =
    SeqexecAppRootModel.sequencesOnDisplayL ^|->> SequencesOnDisplay.sequenceTabs

  val sessionQueueL: Lens[SeqexecAppRootModel, List[SequenceView]] =
    SeqexecAppRootModel.sequences ^|-> SequencesQueue.sessionQueue

  val sessionQueueTableStateL
    : Lens[SeqexecAppRootModel, TableState[SessionQueueTable.TableColumn]] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.appTableStates ^|-> AppTableStates.sessionQueueTable

  def stepsTableStateL(
    id: Observation.Id
  ): Lens[SeqexecAppRootModel, Option[TableState[StepsTable.TableColumn]]] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.appTableStates ^|-> AppTableStates
      .stepsTableAtL(id)

  val soundSettingL: Lens[SeqexecAppRootModel, SoundSelection] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.sound

  val configTableStateL: Lens[SeqexecAppRootModel, TableState[StepConfigTable.TableColumn]] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.appTableStates ^|-> AppTableStates.stepConfigTable

  def executionQueuesT(
    id: QueueId
  ): Traversal[SeqexecAppRootModel, ExecutionQueueView] =
    SeqexecAppRootModel.sequences ^|->
      SequencesQueue.queues ^|->>
      filterIndex((qid: QueueId) => qid === id)

  val queuesT: Traversal[SeqexecAppRootModel, ExecutionQueueView] =
    SeqexecAppRootModel.sequences ^|->
      SequencesQueue.queues ^|->>
      each

  val dayCalG: Getter[SeqexecAppRootModel, Option[ExecutionQueueView]] =
    (SeqexecAppRootModel.sequences ^|->
      SequencesQueue.queues ^|->
      at(CalibrationQueueId)).asGetter

  implicit val eq: Eq[SeqexecAppRootModel] =
    Eq.by(x => (x.sequences, x.ws, x.site, x.clientId, x.uiModel, x.serverVersion))
}
