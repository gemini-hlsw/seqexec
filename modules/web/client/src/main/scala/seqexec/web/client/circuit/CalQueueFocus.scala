// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import scala.collection.immutable.SortedMap

import cats._
import cats.syntax.all._
import monocle.Getter
import monocle.Lens
import monocle.Traversal
import monocle.function.Each.each
import monocle.function.Each.listEach
import monocle.function.FilterIndex.sortedMapFilterIndex
import monocle.macros.Lenses
import seqexec.model.ExecutionQueueView
import seqexec.model.Observation
import seqexec.model.QueueId
import seqexec.model.QueueManipulationOp
import seqexec.web.client.components.queue.CalQueueTable
import seqexec.web.client.model._
import web.client.table.TableState

@Lenses
final case class CalQueueFocus(
  status:     ClientStatus,
  seqs:       List[CalQueueSeq],
  tableState: TableState[CalQueueTable.TableColumn],
  seqOps:     SortedMap[Observation.Id, QueueSeqOperations],
  lastOp:     Option[QueueManipulationOp]
) {
  val canOperate: Boolean = status.canOperate
  val loggedIn: Boolean   = status.isLogged
}

object CalQueueFocus {
  implicit val eq: Eq[CalQueueFocus] =
    Eq.by(x => (x.status, x.seqs, x.tableState, x.seqOps, x.lastOp))

  def seqQueueOpsT(
    id: Observation.Id
  ): Traversal[CalQueueFocus, QueueSeqOperations] =
    CalQueueFocus.seqOps.andThen(
      sortedMapFilterIndex[Observation.Id, QueueSeqOperations].filterIndex((oid: Observation.Id) =>
        oid === id
      )
    )

  // All metadata of the given obs
  def calSeq(
    id: Observation.Id
  ): Getter[SeqexecAppRootModel, Option[CalQueueSeq]] =
    SeqexecAppRootModel.sequences.andThen(CalQueueSeq.calQueueSeqG(id))

  def calTS(
    id: QueueId
  ): Lens[SeqexecAppRootModel, Option[TableState[CalQueueTable.TableColumn]]] =
    SeqexecAppRootModel.uiModel
      .andThen(SeqexecUIModel.appTableStates)
      .andThen(AppTableStates.queueTableAtL(id))

  private def seqOpsL(id: QueueId) =
    SeqexecAppRootModel.uiModel
      .andThen(SeqexecUIModel.queues)
      .andThen(CalibrationQueues.calStateSeqOpsT(id))

  private def qLastOpL(id: QueueId) =
    SeqexecAppRootModel.uiModel
      .andThen(SeqexecUIModel.queues)
      .andThen(CalibrationQueues.calLastOpO(id))

  // A fairly complicated getter
  def calQueueG(
    id: QueueId
  ): Getter[SeqexecAppRootModel, Option[CalQueueFocus]] = {
    // All ids on the queue
    val ids: Traversal[SeqexecAppRootModel, Observation.Id] =
      SeqexecAppRootModel.executionQueuesT(id).andThen(ExecutionQueueView.queue).andThen(each)

    // combine
    val calQueueSeqG = (s: SeqexecAppRootModel) => ids.getAll(s).map(i => calSeq(i).get(s))

    ClientStatus.clientStatusFocusL.asGetter
      .zip(
        Getter(calQueueSeqG).zip(
          calTS(id).asGetter.zip(Getter(seqOpsL(id).getOption).zip(Getter(qLastOpL(id).getOption)))
        )
      ) >>> {
      case (status, (ids, (ts, (seqOps, lastOp)))) =>
        val obsIds = ids.collect { case Some(x) => x }
        CalQueueFocus(status,
                      obsIds,
                      ts.getOrElse(CalQueueTable.State.ROTableState),
                      seqOps.getOrElse(SortedMap.empty),
                      lastOp.flatten
        ).some
      case _                                       =>
        none
    }
  }
}
