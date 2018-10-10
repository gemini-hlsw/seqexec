// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import gem.Observation
import monocle.Optional
import monocle.Traversal
import monocle.macros.Lenses
import monocle.function.At.at
import monocle.function.At.atSortedMap
import monocle.std
import monocle.function.Each.each
import scala.collection.immutable.SortedMap
import seqexec.model.CalibrationQueueId
import seqexec.model.QueueId
import seqexec.web.client.components.queue.CalQueueTable
import web.client.table.TableState

@Lenses
final case class CalQueueState(
  ops:        QueueOperations,
  tableState: TableState[CalQueueTable.TableColumn],
  seqOps:     SortedMap[Observation.Id, QueueSeqOperations])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalQueueState {
  implicit val eq: Eq[CalQueueState] =
    Eq.by(x => (x.ops, x.seqOps, x.tableState))

  val Default: CalQueueState =
    CalQueueState(QueueOperations.Default,
                  CalQueueTable.State.ROTableState,
                  SortedMap.empty)
}

@Lenses
final case class CalibrationQueues(queues: SortedMap[QueueId, CalQueueState]) {
  val queueTables: SortedMap[QueueId, TableState[CalQueueTable.TableColumn]] =
    queues.mapValues(_.tableState)

  def updateTableStates(queueTs: Map[QueueId, TableState[CalQueueTable.TableColumn]]): CalibrationQueues =
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

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalibrationQueues {
  implicit val eq: Eq[CalibrationQueues] =
    Eq.by(_.queues)

  val Default: CalibrationQueues =
    CalibrationQueues(SortedMap(CalibrationQueueId -> CalQueueState.Default))

  def calQueueStateL(qid: QueueId): Optional[CalibrationQueues, QueueOperations] =
    CalibrationQueues.queues ^|->
      at(qid)                ^<-?
      std.option.some        ^|->
      CalQueueState.ops

  def calQueueStateSeqOpsO(qid: QueueId, oid: Observation.Id): Optional[CalibrationQueues, QueueSeqOperations] =
    CalibrationQueues.queues ^|->
      at(qid)                ^<-?
      std.option.some        ^|->
      CalQueueState.seqOps   ^|->
      at(oid)                ^<-?
      std.option.some

  def tableStatesT: Traversal[CalibrationQueues, TableState[CalQueueTable.TableColumn]] =
    CalibrationQueues.queues   ^|->>
      each                     ^|->
      CalQueueState.tableState

  def runCalL(qid: QueueId): Optional[CalibrationQueues, RunCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.runCalRequested

  def stopCalL(qid: QueueId): Optional[CalibrationQueues, StopCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.stopCalRequested

  def addDayCalL(
    qid: QueueId): Optional[CalibrationQueues, AddDayCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.addDayCalRequested

  def clearAllCalL(
    qid: QueueId): Optional[CalibrationQueues, ClearAllCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.clearAllCalRequested

  def addSeqOps(qid: QueueId,
                oid: Observation.Id): CalibrationQueues => CalibrationQueues =
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

}
