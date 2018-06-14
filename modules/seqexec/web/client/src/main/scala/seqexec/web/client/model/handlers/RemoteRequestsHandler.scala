// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{ActionHandler, ActionResult, Effect, ModelRW}
import seqexec.model.Model._
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient
import cats.implicits._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
* Handles actions sending requests to the backend
*/
class RemoteRequestsHandler[M](modelRW: ModelRW[M, Option[ClientID]]) extends ActionHandler(modelRW) with Handlers {
  def handleRequestOperation: PartialFunction[Any, ActionResult[M]] = {
    case RequestRun(s) =>
      val effect = value.map(clientId => Effect(SeqexecWebClient.run(s, clientId).map(r => if (r.error) RunStartFailed(s) else RunStarted(s)))).getOrElse(VoidEffect)
      effectOnly(effect)

    case RequestPause(s) =>
      effectOnly(Effect(SeqexecWebClient.pause(s).map(r => if (r.error) RunPauseFailed(s) else RunPaused(s))))

    case RequestCancelPause(s) =>
      effectOnly(Effect(SeqexecWebClient.cancelPause(s).map(r => if (r.error) RunCancelPauseFailed(s) else RunCancelPaused(s))))

    case RequestStop(id, step) =>
      effectOnly(Effect(SeqexecWebClient.stop(id, step).map(r => if (r.error) RunStopFailed(id) else RunStop(id))))

    case RequestAbort(id, step) =>
      effectOnly(Effect(SeqexecWebClient.abort(id, step).map(r => if (r.error) RunAbortFailed(id) else RunAbort(id))))

    case RequestObsPause(id, step) =>
      effectOnly(Effect(SeqexecWebClient.pauseObs(id, step).map(r => if (r.error) RunObsPauseFailed(id) else RunObsPause(id))))

    case RequestObsResume(id, step) =>
      effectOnly(Effect(SeqexecWebClient.resumeObs(id, step).map(r => if (r.error) RunObsResumeFailed(id) else RunObsPause(id))))

  }

  def handleOperationResult: PartialFunction[Any, ActionResult[M]] = {
    case RunStarted(_) =>
      noChange

    case RunStartFailed(_) =>
      noChange

    case RunPaused(_) =>
      noChange

    case RunPauseFailed(_) =>
      noChange
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleRequestOperation,
      handleOperationResult).combineAll
}
