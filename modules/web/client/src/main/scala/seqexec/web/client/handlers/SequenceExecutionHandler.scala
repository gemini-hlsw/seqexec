// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import cats.syntax.all._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import diode.NoAction
import seqexec.model.Observer
import seqexec.web.client.actions._
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.services.SeqexecWebClient
import seqexec.web.client.circuit.SequencesQueueFocus

/**
 * Handles sequence execution actions
 */
class SequenceExecutionHandler[M](modelRW: ModelRW[M, SequencesQueueFocus])
    extends ActionHandler(modelRW)
    with Handlers[M, SequencesQueueFocus] {

  def handleFlipSkipBreakpoint: PartialFunction[Any, ActionResult[M]] = {
    case FlipSkipStep(sequenceId, step) =>
      val skipRequest = Effect(
        SeqexecWebClient
          .skip(sequenceId, Observer(value.displayName.orEmpty), step.flipSkip)
          .as(NoAction)
      )
      updatedLE(SequencesQueueFocus.sessionQueue.modify(_.collect {
                  case s if s.id === sequenceId => s.flipSkipMarkAtStep(step)
                  case s                        => s
                }),
                skipRequest
      )

    case FlipBreakpointStep(sequenceId, step) =>
      val breakpointRequest = Effect(
        SeqexecWebClient
          .breakpoint(sequenceId, Observer(value.displayName.orEmpty), step.flipBreakpoint)
          .as(NoAction)
      )
      updatedLE(SequencesQueueFocus.sessionQueue.modify(_.collect {
                  case s if s.id === sequenceId => s.flipBreakpointAtStep(step)
                  case s                        => s
                }),
                breakpointRequest
      )
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleFlipSkipBreakpoint).combineAll
}
