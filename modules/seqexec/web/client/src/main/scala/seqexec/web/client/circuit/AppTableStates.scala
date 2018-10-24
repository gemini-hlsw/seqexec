// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Lens
import monocle.macros.Lenses
import monocle.function.At._
import seqexec.model._
import seqexec.web.client.model._
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.sequence.steps.StepsTable
import seqexec.web.client.components.SessionQueueTableBody
import seqexec.web.client.components.queue.CalQueueTable
import web.client.table._

@Lenses
final case class AppTableStates(
  queueTable:      TableState[SessionQueueTableBody.TableColumn],
  stepConfigTable: TableState[StepConfigTable.TableColumn],
  stepsTables:     Map[Observation.Id, TableState[StepsTable.TableColumn]],
  queueTables:     Map[QueueId, TableState[CalQueueTable.TableColumn]])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object AppTableStates {
  implicit val eq: Eq[AppTableStates] =
    Eq.by(x => (x.queueTable, x.stepConfigTable, x.stepsTables))

  val tableStateL: Lens[SeqexecUIModel, AppTableStates] =
    Lens[SeqexecUIModel, AppTableStates](
      m =>
        AppTableStates(m.queueTableState,
                       m.configTableState,
                       m.sequencesOnDisplay.stepsTables,
                       m.queues.queueTables))(
      v =>
        m =>
          m.copy(
            queueTableState  = v.queueTable,
            configTableState = v.stepConfigTable,
            sequencesOnDisplay = m.sequencesOnDisplay
              .updateTableStates(v.stepsTables),
            queues = m.queues
              .updateTableStates(v.queueTables)
      ))

  def stepTableAtL(id: Observation.Id): Lens[AppTableStates, Option[TableState[StepsTable.TableColumn]]] =
    AppTableStates.stepsTables ^|-> at(id)

  def queueTableAtL(id: QueueId): Lens[AppTableStates, Option[TableState[CalQueueTable.TableColumn]]] =
    AppTableStates.queueTables ^|-> at(id)
}
