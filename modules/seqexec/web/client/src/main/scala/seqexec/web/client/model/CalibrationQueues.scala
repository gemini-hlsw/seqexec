// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import monocle.Optional
import monocle.macros.Lenses
import monocle.function.At.at
import monocle.std
import seqexec.model.CalibrationQueueId
import seqexec.model.QueueId
import seqexec.web.client.components.queue.CalQueueTable
import web.client.table.TableState

@Lenses
final case class CalQueueState(
  ops:        QueueOperations,
  tableState: TableState[CalQueueTable.TableColumn])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalQueueState {
  implicit val eq: Eq[CalQueueState] =
    Eq.by(x => (x.ops, x.tableState))

  val Default: CalQueueState =
    CalQueueState(QueueOperations.Default,
                  CalQueueTable.State.InitialTableState)
}

@Lenses
final case class CalibrationQueues(queues: Map[QueueId, CalQueueState]) {
  val queueTables: Map[QueueId, TableState[CalQueueTable.TableColumn]] =
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
    CalibrationQueues(Map(CalibrationQueueId -> CalQueueState.Default))

  def calQueueStateL(qid: QueueId): Optional[CalibrationQueues, QueueOperations] =
    CalibrationQueues.queues ^|->
      at(qid)                ^<-?
      std.option.some        ^|->
      CalQueueState.ops

  def runCalL(qid: QueueId): Optional[CalibrationQueues, RunCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.runCalRequested

  def stopCalL(qid: QueueId): Optional[CalibrationQueues, StopCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.stopCalRequested

  def addDayCalL(qid: QueueId): Optional[CalibrationQueues, AddDayCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.addDayCalRequested

  def clearAllCalL(qid: QueueId): Optional[CalibrationQueues, ClearAllCalOperation] =
    calQueueStateL(qid) ^|-> QueueOperations.clearAllCalRequested

}
