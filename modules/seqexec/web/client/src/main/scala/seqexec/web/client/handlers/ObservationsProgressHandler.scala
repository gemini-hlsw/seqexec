// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.web.client.model._
import seqexec.web.client.actions._
import seqexec.model.events.ObservationProgressEvent

/**
  * Handles updates to obs progress
  */
class ObservationsProgressHandler[M](
  modelRW: ModelRW[M, ObservationsProgress])
    extends ActionHandler(modelRW)
    with Handlers[M, ObservationsProgress] {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ObservationProgressEvent(e)) =>
      updatedL(ObservationsProgress.progressByIdL(e.obsId).set(e.some))
    }
}
