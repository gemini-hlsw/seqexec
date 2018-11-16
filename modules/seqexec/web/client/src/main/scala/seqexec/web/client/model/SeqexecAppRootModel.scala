// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import gem.enum.Site
import monocle.Lens
import monocle.Getter
import monocle.Traversal
import monocle.macros.Lenses
import monocle.function.At.at
import monocle.function.At.atSortedMap
import monocle.function.Each.each
import monocle.function.FilterIndex.filterIndex
import scala.collection.immutable.SortedMap
import seqexec.model.ClientId
import seqexec.model.Conditions
import seqexec.model.ExecutionQueueView
import seqexec.model.QueueId
import seqexec.model.SequenceView
import seqexec.model.SequencesQueue
import seqexec.model.CalibrationQueueId
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.SessionQueueTable
import web.client.table._

/**
  * Root of the UI Model of the application
  */
@Lenses
final case class SeqexecAppRootModel(sequences: SequencesQueue[SequenceView],
                                     ws:        WebSocketConnection,
                                     site:      Option[Site],
                                     clientId:  Option[ClientId],
                                     uiModel:   SeqexecUIModel)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SeqexecAppRootModel {
  val NoSequencesLoaded: SequencesQueue[SequenceView] =
    SequencesQueue[SequenceView](Map.empty,
                                 Conditions.Default,
                                 None,
                                 SortedMap.empty,
                                 Nil)

  val Initial: SeqexecAppRootModel = SeqexecAppRootModel(
    NoSequencesLoaded,
    WebSocketConnection.Empty,
    None,
    None,
    SeqexecUIModel.Initial)

  val logDisplayL: Lens[SeqexecAppRootModel, SectionVisibilityState] =
    SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.globalLog ^|->
      GlobalLog.display

  val sessionQueueFilterL: Lens[SeqexecAppRootModel, SessionQueueFilter] =
    SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.sessionQueueFilter

  val sequencesOnDisplayL: Lens[SeqexecAppRootModel, SequencesOnDisplay] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.sequencesOnDisplay

  val sessionQueueL: Lens[SeqexecAppRootModel, List[SequenceView]] =
    SeqexecAppRootModel.sequences ^|-> SequencesQueue.sessionQueue

  val queueTableStateL
    : Lens[SeqexecAppRootModel, TableState[SessionQueueTable.TableColumn]] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.queueTableState

  val soundSettingL: Lens[SeqexecAppRootModel, SoundSelection] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.sound

  val configTableStateL
    : Lens[SeqexecAppRootModel, TableState[StepConfigTable.TableColumn]] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.configTableState

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
    Eq.by(x => (x.sequences, x.ws, x.site, x.clientId, x.uiModel))
}
