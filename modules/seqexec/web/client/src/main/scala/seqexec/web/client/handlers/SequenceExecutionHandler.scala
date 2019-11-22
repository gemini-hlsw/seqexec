// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import diode.NoAction
import seqexec.model.SequencesQueue
import seqexec.model.SequenceView
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
  * Handles sequence execution actions
  */
class SequenceExecutionHandler[M](
  modelRW: ModelRW[M, SequencesQueue[SequenceView]])
    extends ActionHandler(modelRW)
    with Handlers[M, SequencesQueue[SequenceView]] {
  def handleUpdateObserver: PartialFunction[Any, ActionResult[M]] = {
    case UpdateObserver(sequenceId, name) =>
      val updateObserverE = Effect(
        SeqexecWebClient.setObserver(sequenceId, name.value).as(NoAction)
        ) + Effect.action(UpdateDefaultObserver(name))
      val updatedSequences =
        value.copy(sessionQueue = value.sessionQueue.collect {
          case s if s.id === sequenceId =>
            s.copy(metadata = s.metadata.copy(observer = Some(name)))
          case s => s
        })
      updated(updatedSequences, updateObserverE)
  }

  def handleFlipSkipBreakpoint: PartialFunction[Any, ActionResult[M]] = {
    case FlipSkipStep(sequenceId, step) =>
      val skipRequest = Effect(
        SeqexecWebClient.skip(sequenceId, step.flipSkip).as(NoAction))
      updated(value.copy(sessionQueue = value.sessionQueue.collect {
        case s if s.id === sequenceId => s.flipSkipMarkAtStep(step)
        case s                        => s
      }), skipRequest)

    case FlipBreakpointStep(sequenceId, step) =>
      val breakpointRequest = Effect(
        SeqexecWebClient
          .breakpoint(sequenceId, step.flipBreakpoint)
          .as(NoAction))
      updated(value.copy(sessionQueue = value.sessionQueue.collect {
        case s if s.id === sequenceId => s.flipBreakpointAtStep(step)
        case s                        => s
      }), breakpointRequest)
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleUpdateObserver, handleFlipSkipBreakpoint).combineAll
}
