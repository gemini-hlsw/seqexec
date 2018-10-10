// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import gem.enum.Site
import monocle.Lens
import monocle.Traversal
import monocle.macros.Lenses
import monocle.function.FilterIndex.filterIndex
import monocle.unsafe.MapTraversal._
import seqexec.model.{ClientId, Conditions, ExecutionQueueView, QueueId, SequenceView, SequencesQueue}
import seqexec.web.client.components.sequence.steps.StepConfigTable
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
                                 Map.empty,
                                 Nil)

  val Initial: SeqexecAppRootModel = SeqexecAppRootModel(
    NoSequencesLoaded,
    WebSocketConnection.Empty,
    None,
    None,
    SeqexecUIModel.Initial)

  val logDisplayL: Lens[SeqexecAppRootModel, SectionVisibilityState] =
    SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.globalLog  ^|->
      GlobalLog.display

  val sequencesOnDisplayL: Lens[SeqexecAppRootModel, SequencesOnDisplay] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.sequencesOnDisplay

  val configTableStateL: Lens[SeqexecAppRootModel, TableState[StepConfigTable.TableColumn]] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.configTableState

  def executionQueuesT(
    id: QueueId): Traversal[SeqexecAppRootModel, ExecutionQueueView] =
    SeqexecAppRootModel.sequences ^|->
      SequencesQueue.queues       ^|->>
      filterIndex((qid: QueueId) => qid === id)

  implicit val eq: Eq[SeqexecAppRootModel] =
    Eq.by(x => (x.sequences, x.ws, x.site, x.clientId, x.uiModel))
}
