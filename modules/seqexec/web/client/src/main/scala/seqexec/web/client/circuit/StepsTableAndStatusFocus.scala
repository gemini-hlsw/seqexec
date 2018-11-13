// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Getter
import seqexec.web.client.model._
import seqexec.web.client.components.sequence.steps.StepConfigTable
import web.client.table._

final case class StepsTableAndStatusFocus(
  status:           ClientStatus,
  stepsTable:       Option[StepsTableFocus],
  configTableState: TableState[StepConfigTable.TableColumn])

object StepsTableAndStatusFocus {
  implicit val eq: Eq[StepsTableAndStatusFocus] =
    Eq.by(x => (x.status, x.stepsTable, x.configTableState))

  def stepsTableAndStatusFocusG(
    id: Observation.Id): Getter[SeqexecAppRootModel, StepsTableAndStatusFocus] =
    ClientStatus.clientStatusFocusL.asGetter
      .zip(
        StepsTableFocus
          .stepsTableG(id)
          .zip(SeqexecAppRootModel.configTableStateL.asGetter)) >>> {
      case (s, (f, t)) => StepsTableAndStatusFocus(s, f, t)
    }

}
