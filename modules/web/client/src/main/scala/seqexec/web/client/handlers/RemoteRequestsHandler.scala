// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import cats.syntax.all._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import seqexec.web.client.actions._
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.services.SeqexecWebClient

/**
 * Handles actions sending requests to the backend
 */
class RemoteRequestsHandler[M](modelRW: ModelRW[M, ClientStatus])
    extends ActionHandler(modelRW)
    with Handlers[M, ClientStatus] {

  def handleRun: PartialFunction[Any, ActionResult[M]] = { case RequestRun(s, options) =>
    val effect = (value.clientId, value.observer)
      .mapN((clientId, observer) =>
        Effect(
          SeqexecWebClient
            .run(s, observer, clientId, options)
            .as(RunStarted(s))
            .recover { case _ =>
              RunStartFailed(s)
            }
        )
      )
      .getOrElse(VoidEffect)
    effectOnly(effect)
  }

  def handlePause: PartialFunction[Any, ActionResult[M]] = { case RequestPause(id) =>
    val effect = (value.clientId, value.observer)
      .mapN((clientId, observer) =>
        requestEffect(id,
                      SeqexecWebClient.pause(_, observer),
                      RunPaused.apply,
                      RunPauseFailed.apply
        )
      )
      .getOrElse(VoidEffect)
    effectOnly(effect)
  }

  def handleRunFrom: PartialFunction[Any, ActionResult[M]] = {
    case RequestRunFrom(id, stepId, options) =>
      val effect = (value.clientId, value.observer)
        .mapN((clientId, observer) =>
          requestEffect(id,
                        SeqexecWebClient.runFrom(_, stepId, observer, clientId, options),
                        RunFromComplete(_, stepId),
                        RunFromFailed(_, stepId)
          )
        )
        .getOrElse(VoidEffect)
      effectOnly(effect)
  }

  def handleCancelPause: PartialFunction[Any, ActionResult[M]] = { case RequestCancelPause(id) =>
    val effect = (value.clientId, value.observer)
      .mapN((clientId, observer) =>
        requestEffect(id,
                      SeqexecWebClient.cancelPause(_, observer),
                      RunCancelPaused.apply,
                      RunCancelPauseFailed.apply
        )
      )
      .getOrElse(VoidEffect)
    effectOnly(effect)
  }

  def handleStop: PartialFunction[Any, ActionResult[M]] = { case RequestStop(id, step) =>
    effectOnly(
      (value.clientId, value.observer)
        .mapN((clientId, observer) =>
          Effect(
            SeqexecWebClient
              .stop(id, observer, step)
              .as(RunStop(id))
              .recover { case _ =>
                RunStopFailed(id)
              }
          )
        )
        .getOrElse(VoidEffect)
    )
  }

  def handleGracefulStop: PartialFunction[Any, ActionResult[M]] = {
    case RequestGracefulStop(id, step) =>
      effectOnly(
        (value.clientId, value.observer)
          .mapN((clientId, observer) =>
            Effect(
              SeqexecWebClient
                .stopGracefully(id, observer, step)
                .as(RunGracefulStop(id))
                .recover { case _ =>
                  RunGracefulStopFailed(id)
                }
            )
          )
          .getOrElse(VoidEffect)
      )
  }

  def handleAbort: PartialFunction[Any, ActionResult[M]] = { case RequestAbort(id, step) =>
    effectOnly(
      (value.clientId, value.observer)
        .mapN((clientId, observer) =>
          Effect(
            SeqexecWebClient
              .abort(id, observer, step)
              .as(RunAbort(id))
              .recover { case _ =>
                RunAbortFailed(id)
              }
          )
        )
        .getOrElse(VoidEffect)
    )
  }

  def handleObsPause: PartialFunction[Any, ActionResult[M]] = { case RequestObsPause(id, step) =>
    effectOnly(
      (value.clientId, value.observer)
        .mapN((clientId, observer) =>
          Effect(
            SeqexecWebClient
              .pauseObs(id, observer, step)
              .as(RunObsPause(id))
              .recover { case _ =>
                RunObsPauseFailed(id)
              }
          )
        )
        .getOrElse(VoidEffect)
    )
  }

  def handleGracefulObsPause: PartialFunction[Any, ActionResult[M]] = {
    case RequestGracefulObsPause(id, step) =>
      effectOnly(
        (value.clientId, value.observer)
          .mapN((clientId, observer) =>
            Effect(
              SeqexecWebClient
                .pauseObsGracefully(id, observer, step)
                .as(RunGracefulObsPause(id))
                .recover { case _ =>
                  RunGracefulObsPauseFailed(id)
                }
            )
          )
          .getOrElse(VoidEffect)
      )
  }

  def handleObsResume: PartialFunction[Any, ActionResult[M]] = { case RequestObsResume(id, step) =>
    effectOnly(
      (value.clientId, value.observer)
        .mapN((clientId, observer) =>
          Effect(
            SeqexecWebClient
              .resumeObs(id, observer, step)
              .as(RunObsResume(id))
              .recover { case _ =>
                RunObsResumeFailed(id)
              }
          )
        )
        .getOrElse(VoidEffect)
    )
  }

  def handleSync: PartialFunction[Any, ActionResult[M]] = { case RequestSync(id) =>
    effectOnly(requestEffect(id, SeqexecWebClient.sync, RunSync.apply, RunSyncFailed.apply))
  }

  def handleResourceRun: PartialFunction[Any, ActionResult[M]] = {
    case RequestResourceRun(id, step, resource) =>
      val effect = (value.clientId, value.observer)
        .mapN((clientId, observer) =>
          requestEffect(
            id,
            SeqexecWebClient.runResource(step, resource, observer, _, clientId),
            RunResource(_, step, resource),
            RunResourceFailed(_, step, resource, s"Http call to configure ${resource.show} failed")
          )
        )
        .getOrElse(VoidEffect)
      effectOnly(effect)
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(
      handleRun,
      handlePause,
      handleCancelPause,
      handleStop,
      handleGracefulStop,
      handleAbort,
      handleObsPause,
      handleGracefulObsPause,
      handleObsResume,
      handleSync,
      handleRunFrom,
      handleResourceRun
    ).combineAll

}
