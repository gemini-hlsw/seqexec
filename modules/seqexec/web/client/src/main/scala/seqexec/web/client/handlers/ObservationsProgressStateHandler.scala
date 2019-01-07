// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.model.enum.ActionStatus
import seqexec.model.events.ObservationProgressEvent
import seqexec.model.events.StepExecuted
import seqexec.web.client.model._
import seqexec.web.client.actions._
import seqexec.web.client.model.lenses.sequenceStepT
import seqexec.web.client.model.lenses.sequenceViewT
import seqexec.web.client.model.ModelOps._

/**
  * Handles updates to obs progress
  */
class ObservationsProgressStateHandler[M](
  modelRW: ModelRW[M, AllObservationsProgressState])
    extends ActionHandler(modelRW)
    with Handlers[M, AllObservationsProgressState] {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ObservationProgressEvent(e)) =>
      updatedL(
        AllObservationsProgressState
          .progressByIdL(e.obsId, e.stepId)
          .set(e.some))

    // Remove the progress once the step completes
    case ServerMessage(e @ StepExecuted(obsId, _)) =>
      val upd =
        for {
          obs     <- sequenceViewT.find(_.id === obsId)(e)
          curSIdx <- obs.runningStep.map(_.last)
          curStep <- sequenceStepT.find(_.id === curSIdx)(obs)
        } yield
          if (curStep.observeStatus === ActionStatus.Completed && !curStep.isObservePaused) {
            updatedL(
              AllObservationsProgressState
                .progressByIdL(e.obsId, curSIdx)
                .set(none))
          } else {
            noChange
          }

      upd.getOrElse(noChange)
  }
}
