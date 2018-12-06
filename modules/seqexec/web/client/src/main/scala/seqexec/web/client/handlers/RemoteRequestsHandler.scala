// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import gem.Observation
import seqexec.model.ClientId
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
  * Handles actions sending requests to the backend
  */
class RemoteRequestsHandler[M](modelRW: ModelRW[M, Option[ClientId]])
    extends ActionHandler(modelRW)
    with Handlers[M, Option[ClientId]] {

  def handleRun: PartialFunction[Any, ActionResult[M]] = {
    case RequestRun(s) =>
      val effect = value
        .map(
          clientId =>
            Effect(
              SeqexecWebClient
                .run(s, clientId)
                .map(_ => RunStarted(s))
                .recover {
                  case _ => RunStartFailed(s)
                }))
        .getOrElse(VoidEffect)
      effectOnly(effect)
  }

  def handlePause: PartialFunction[Any, ActionResult[M]] = {
    case RequestPause(id) =>
      effectOnly(
        requestEffect(id,
                      SeqexecWebClient.pause,
                      RunPaused.apply,
                      RunPauseFailed.apply))
  }

  def handleCancelPause: PartialFunction[Any, ActionResult[M]] = {
    case RequestCancelPause(id) =>
      effectOnly(
        requestEffect(id,
                      SeqexecWebClient.cancelPause,
                      RunCancelPaused.apply,
                      RunCancelPauseFailed.apply))
  }

  def handleStop: PartialFunction[Any, ActionResult[M]] = {
    case RequestStop(id, step) =>
      effectOnly(
        Effect(
          SeqexecWebClient
            .stop(id, step)
            .map(_ => RunStop(id))
            .recover {
              case _ => RunStopFailed(id)
            }))
  }

  def handleAbort: PartialFunction[Any, ActionResult[M]] = {
    case RequestAbort(id, step) =>
      effectOnly(
        Effect(
          SeqexecWebClient
            .abort(id, step)
            .map(_ => RunAbort(id))
            .recover {
              case _ => RunAbortFailed(id)
            }))
  }

  def handleObsPause: PartialFunction[Any, ActionResult[M]] = {
    case RequestObsPause(id, step) =>
      effectOnly(
        Effect(
          SeqexecWebClient
            .pauseObs(id, step)
            .map(_ => RunObsPause(id))
            .recover {
              case _ => RunObsPauseFailed(id)
            }))
  }

  def handleObsResume: PartialFunction[Any, ActionResult[M]] = {
    case RequestObsResume(id, step) =>
      effectOnly(
        Effect(
          SeqexecWebClient
            .resumeObs(id, step)
            .map(_ => RunObsPause(id))
            .recover {
              case _ => RunObsResumeFailed(id)
            }))
  }

  def handleSync: PartialFunction[Any, ActionResult[M]] = {
    case RequestSync(id) =>
      effectOnly(
        requestEffect(id,
                      SeqexecWebClient.sync,
                      RunSync.apply,
                      RunSyncFailed.apply))
  }

  def handleResourceRun: PartialFunction[Any, ActionResult[M]] = {
    case RequestResourceRun(id, step, resource) =>
      effectOnly(
        requestEffect(
          id,
          SeqexecWebClient.runResource(step, resource),
          (id: Observation.Id) => RunResource(id, step, resource),
          (id: Observation.Id) => RunResourceFailed(id, step, resource)
        ))
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleRun,
         handlePause,
         handleCancelPause,
         handleStop,
         handleAbort,
         handleObsPause,
         handleObsResume,
         handleSync,
         handleResourceRun).combineAll

}
