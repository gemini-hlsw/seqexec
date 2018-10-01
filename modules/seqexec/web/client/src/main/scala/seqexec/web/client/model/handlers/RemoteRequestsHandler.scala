// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.Action
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import seqexec.model.ClientID
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
  * Handles actions sending requests to the backend
  */
class RemoteRequestsHandler[M](modelRW: ModelRW[M, Option[ClientID]])
    extends ActionHandler(modelRW)
    with Handlers[M, Option[ClientID]] {

  def handleRun: PartialFunction[Any, ActionResult[M]] = {
    case RequestRun(s) =>
      val effect = value
        .map(
          clientId =>
            Effect(
              SeqexecWebClient
                .run(s, clientId)
                .map(r => RunStarted(s))
                .recover {
                  case _ => RunStartFailed(s)
                }))
        .getOrElse(VoidEffect)
      effectOnly(effect)
  }

  private def requestEffect[A, B <: Action, C <: Action](
      a: A,
      f: A => Future[Unit],
      m: A => B,
      r: A => C): Effect =
    Effect(
      f(a)
        .map(_ => m(a))
        .recover {
          case _ => r(a)
        }
    )

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
            .map(r => RunStop(id))
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
            .map(r => RunAbort(id))
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
            .map(r => RunObsPause(id))
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
            .map(r => RunObsPause(id))
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

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleRun,
         handlePause,
         handleCancelPause,
         handleStop,
         handleAbort,
         handleObsPause,
         handleObsResume,
         handleSync).combineAll

}
