// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import monocle.macros.Lenses
import seqexec.model.BatchCommandState
import seqexec.model.Observation

@Lenses
final case class ExecutionQueue(
  name:     String,
  cmdState: BatchCommandState,
  queue:    List[Observation.Id]
)

object ExecutionQueue {
  def init(name: String): ExecutionQueue =
    ExecutionQueue(name, BatchCommandState.Idle, List.empty)

  implicit val eq: Eq[ExecutionQueue] =
    Eq.by(x => (x.name, x.cmdState, x.queue))
}
