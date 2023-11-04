// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import monocle.Lens
import monocle.function.At._
import monocle.macros.Lenses
import seqexec.model.Observation
import seqexec.model._
import seqexec.web.client.components.SessionQueueTable
import seqexec.web.client.components.queue.CalQueueTable
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.sequence.steps.StepsTable
import web.client.table._

/**
 * Store the state of each of the app tables
 */
@Lenses
final case class AppTableStates(
  sessionQueueTable: TableState[SessionQueueTable.TableColumn],
  stepConfigTable:   TableState[StepConfigTable.TableColumn],
  stepsTables:       Map[Observation.Id, TableState[StepsTable.TableColumn]],
  queueTables:       Map[QueueId, TableState[CalQueueTable.TableColumn]]
)

object AppTableStates {

  val Initial: AppTableStates = AppTableStates(
    SessionQueueTable.State.InitialTableState,
    StepConfigTable.InitialTableState,
    Map.empty,
    Map.empty
  )

  implicit val eq: Eq[AppTableStates] =
    Eq.by(x => (x.sessionQueueTable, x.stepConfigTable, x.stepsTables, x.queueTables))

  def stepsTableAtL(
    id: Observation.Id
  ): Lens[AppTableStates, Option[TableState[StepsTable.TableColumn]]] =
    AppTableStates.stepsTables.andThen(at(id))

  def queueTableAtL(
    id: QueueId
  ): Lens[AppTableStates, Option[TableState[CalQueueTable.TableColumn]]] =
    AppTableStates.queueTables.andThen(at(id))
}
