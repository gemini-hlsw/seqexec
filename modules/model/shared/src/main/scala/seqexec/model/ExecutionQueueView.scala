// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.syntax.all._
import monocle.macros.Lenses
import seqexec.model.Observation
import seqexec.model.enum.BatchExecState

@Lenses
final case class ExecutionQueueView(id:        QueueId,
                                    name:      String,
                                    cmdState:  BatchCommandState,
                                    execState: BatchExecState,
                                    queue:     List[Observation.Id]) {
  val observer: Option[Observer] = cmdState match {
    case BatchCommandState.Run(o, _, _) => o.some
    case _                              => none
  }
}

object ExecutionQueueView {
  implicit val eq: Eq[ExecutionQueueView] =
    Eq.by(x => (x.id, x.name, x.cmdState, x.execState, x.queue))

}
