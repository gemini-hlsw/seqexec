// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import scala.collection.immutable.SortedMap

import cats._
import cats.syntax.all._
import monocle.Optional
import monocle.Traversal
import monocle.function.At.at
import monocle.function.At.atSortedMap
import monocle.function.Each.each
import monocle.macros.Lenses
import monocle.std
import seqexec.model.CalibrationQueueId
import seqexec.model.Observation
import seqexec.model.QueueId
import seqexec.model.QueueManipulationOp
import seqexec.web.client.components.queue.CalQueueTable
import web.client.table.TableState

@Lenses
final case class CalQueueState(
  ops:        QueueOperations,
  tableState: TableState[CalQueueTable.TableColumn],
  seqOps:     SortedMap[Observation.Id, QueueSeqOperations],
  lastOp:     Option[QueueManipulationOp])

object CalQueueState {
  implicit val eq: Eq[CalQueueState] =
    Eq.by(x => (x.ops, x.seqOps, x.tableState, x.lastOp))

  val Default: CalQueueState =
    CalQueueState(QueueOperations.Default,
                  CalQueueTable.State.ROTableState,
                  SortedMap.empty,
                  None)
}

@Lenses
final case class CalibrationQueues(queues: SortedMap[QueueId, CalQueueState]) {
  val queueTables: SortedMap[QueueId, TableState[CalQueueTable.TableColumn]] =
    queues.view.mapValues(_.tableState).to(SortedMap)

  def updateTableStates(
    queueTs: Map[QueueId, TableState[CalQueueTable.TableColumn]]
  ): CalibrationQueues =
    copy(queues = queues.map {
      case (i, st) if queueTs.contains(i) =>
        (i,
         queueTs
           .get(i)
           .map(CalQueueState.tableState.set)
           .map(_(st))
           .getOrElse(st))
      case i => i
    })
}

object CalibrationQueues {
  implicit val eq: Eq[CalibrationQueues] =
    Eq.by(_.queues)

  val Default: CalibrationQueues =
    CalibrationQueues(SortedMap(CalibrationQueueId -> CalQueueState.Default))

  def queueO(
    qid: QueueId
  ): Optional[CalibrationQueues, CalQueueState] =
    CalibrationQueues.queues ^|->
      at(qid)                ^<-?
      std.option.some

  def calQueueStateL(
    qid: QueueId
  ): Optional[CalibrationQueues, QueueOperations] =
    queueO(qid) ^|-> CalQueueState.ops

  def calLastOpO(
    qid: QueueId
  ): Optional[CalibrationQueues, Option[QueueManipulationOp]] =
    queueO(qid) ^|-> CalQueueState.lastOp

  def calQueueStateSeqOpsO(
    qid: QueueId,
    oid: Observation.Id
  ): Optional[CalibrationQueues, QueueSeqOperations] =
    queueO(qid)            ^|->
      CalQueueState.seqOps ^|->
      at(oid)              ^<-?
      std.option.some

  def calStateSeqOpsT(
    qid: QueueId
  ): Optional[CalibrationQueues,
              SortedMap[Observation.Id, QueueSeqOperations]] =
    queueO(qid) ^|-> CalQueueState.seqOps

  def tableStatesT
    : Traversal[CalibrationQueues, TableState[CalQueueTable.TableColumn]] =
    CalibrationQueues.queues   ^|->>
      each                     ^|->
      CalQueueState.tableState

  def runCalL(qid: QueueId): Optional[CalibrationQueues, RunCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.runCalRequested

  def stopCalL(qid: QueueId): Optional[CalibrationQueues, StopCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.stopCalRequested

  def addDayCalL(
    qid: QueueId
  ): Optional[CalibrationQueues, AddDayCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.addDayCalRequested

  def clearAllCalL(
    qid: QueueId
  ): Optional[CalibrationQueues, ClearAllCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.clearAllCalRequested

  def addSeqOps(
    qid: QueueId,
    oid: Observation.Id
  ): CalibrationQueues => CalibrationQueues =
    c =>
      c.copy(queues = c.queues.map {
        case (i, st) if qid === i =>
          val so = if (st.seqOps.contains(oid)) {
            st.seqOps
          } else {
            st.seqOps + (oid -> QueueSeqOperations.Default)
          }
          (i, st.copy(seqOps = so))
        case i => i
      })

  def modifyOrAddSeqOps(
    qid: QueueId,
    oid: Observation.Id,
    m:   QueueSeqOperations => QueueSeqOperations
  ): CalibrationQueues => CalibrationQueues =
    addSeqOps(qid, oid) >>> calQueueStateSeqOpsO(qid, oid).modify(m)

  def modifyAllSeqOps(
    qid:  QueueId,
    oids: List[Observation.Id],
    m:    QueueSeqOperations => QueueSeqOperations
  ): CalibrationQueues => CalibrationQueues =
    calStateSeqOpsT(qid).modify {
      _.map {
        case (i, s) if oids.contains(i) => (i, m(s))
        case i                          => i
      }
    }

  def removeSeqOps(
    qid:  QueueId,
    oids: List[Observation.Id]
  ): CalibrationQueues => CalibrationQueues =
    calStateSeqOpsT(qid).modify(_.view.filterKeys(!oids.contains(_)).to(SortedMap))

}
