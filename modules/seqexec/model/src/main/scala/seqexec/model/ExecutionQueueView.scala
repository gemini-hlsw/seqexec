// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.macros.Lenses
import seqexec.model.enum.{BatchCommandState, BatchExecState}

@Lenses
final case class ExecutionQueueView(id:        QueueId,
                                    name:      String,
                                    cmdState:  BatchCommandState,
                                    execState: BatchExecState,
                                    queue:     List[Observation.Id])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object ExecutionQueueView {
  implicit val eq: Eq[ExecutionQueueView] =
    Eq.by(x => (x.id, x.name, x.cmdState, x.execState, x.queue))
}
