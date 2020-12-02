// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.Eq
import seqexec.model.Observation
import seqexec.model.StepId

final case class ActionIndex(self: Long) extends AnyVal
object ActionIndex {
  implicit val actionIndexEq: Eq[ActionIndex] = Eq.by(_.self)
}

final case class ExecutionIndex(self: Long) extends AnyVal
object ExecutionIndex {
  implicit val actionIndexEq: Eq[ExecutionIndex] = Eq.by(_.self)
}

final case class ActionCoordsInSeq(stepId: StepId,
                                   execIdx: ExecutionIndex,
                                   actIdx: ActionIndex)
object ActionCoordsInSeq {
  implicit val actionCoordsInSeqEq: Eq[ActionCoordsInSeq] = Eq.by(x =>
    (x.stepId, x.execIdx, x.actIdx)
  )
}

/*
 * Class to hold the coordinates of an Action inside the engine state
 */
final case class ActionCoords(sid: Observation.Id, actCoords: ActionCoordsInSeq)
object ActionCoords {
  implicit val actionCoordsEq: Eq[ActionCoords] = Eq.by(x => (x.sid, x.actCoords))
}
