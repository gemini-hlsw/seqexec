// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Getter
import seqexec.web.client.model._
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.sequence.steps.StepsTable
import web.client.table._

final case class StepsTableAndStatusFocus(
  status:           ClientStatus,
  stepsTable:       Option[StepsTableFocus],
  tableState:       TableState[StepsTable.TableColumn],
  configTableState: TableState[StepConfigTable.TableColumn])

object StepsTableAndStatusFocus {
  implicit val eq: Eq[StepsTableAndStatusFocus] =
    Eq.by(x => (x.status, x.stepsTable, x.tableState, x.configTableState))

  def stepsTableAndStatusFocusG(
    id: Observation.Id): Getter[SeqexecAppRootModel, StepsTableAndStatusFocus] =
    ClientStatus.clientStatusFocusL.asGetter
      .zip(
        StepsTableFocus
          .stepsTableG(id)
          .zip(SeqexecAppRootModel.stepsTableStateL(id).asGetter.zip(SeqexecAppRootModel.configTableStateL.asGetter))) >>> {
      case (s, (f, (a, t))) => StepsTableAndStatusFocus(s, f, a.getOrElse(StepsTable.State.InitialTableState), t)
    }

}
