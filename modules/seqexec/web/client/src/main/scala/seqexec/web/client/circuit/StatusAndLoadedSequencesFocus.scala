// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.Order
import cats.implicits._
import diode._
import gem.Observation
import seqexec.model._
import seqexec.model.enum._
import seqexec.web.client.model._
import seqexec.web.client.components.SessionQueueTableBody
import web.client.table._

final case class SequenceInSessionQueue(id:            Observation.Id,
                                        status:        SequenceState,
                                        instrument:    Instrument,
                                        active:        Boolean,
                                        loaded:        Boolean,
                                        name:          String,
                                        targetName:    Option[TargetName],
                                        runningStep:   Option[RunningStep],
                                        nextStepToRun: Option[Int])

object SequenceInSessionQueue {
  implicit val order: Order[SequenceInSessionQueue] = Order.by(_.id)
  implicit val ordering: scala.math.Ordering[SequenceInSessionQueue] =
    order.toOrdering
}

final case class StatusAndLoadedSequencesFocus(
  status:     ClientStatus,
  sequences:  List[SequenceInSessionQueue],
  tableState: TableState[SessionQueueTableBody.TableColumn])
    extends UseValueEq

object StatusAndLoadedSequencesFocus {
  implicit val eq: Eq[StatusAndLoadedSequencesFocus] =
    Eq.by(x => (x.status, x.sequences, x.tableState))

}
