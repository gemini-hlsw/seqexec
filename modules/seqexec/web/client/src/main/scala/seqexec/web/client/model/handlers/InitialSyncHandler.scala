// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{ActionHandler, ActionResult, Effect, ModelRW}
import seqexec.model.SequenceView
import seqexec.model.events.SeqexecModelUpdate
import seqexec.web.client.actions._
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages._
import scala.concurrent.Future
import cats.implicits._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * This handler is called only once. It will be triggered when the first message
 * with the full model arrives.
 * Then we sync to the first running sequence or to the route we are currently on
 */
class InitialSyncHandler[M](modelRW: ModelRW[M, InitialSyncFocus]) extends ActionHandler(modelRW) with Handlers {
  def runningSequence(s: SeqexecModelUpdate): Option[SequenceView] =
    s.view.queue.filter(_.status.isRunning).sortBy(_.id).headOption

  def handle: PartialFunction[Any, ActionResult[M]] = {
    // If there is a running sequence update the page to go there
    case ServerMessage(s: SeqexecModelUpdate) if runningSequence(s).isDefined && value.firstLoad =>
      val running = runningSequence(s)
      running.fold(updated(value.copy(firstLoad = true))) { f =>
        val effect = Effect(Future(SelectIdToDisplay(f.metadata.instrument, f.id)))
        updated(value.copy(firstLoad = false), effect)
      }

    // Otherwise, update the model to reflect the current page
    case ServerMessage(s: SeqexecModelUpdate) if value.firstLoad                                 =>
      // the page maybe not in sync with the tabs. Let's fix that
      val sids = s.view.queue.map(_.id)
      val effect = value.location match {
        case SequencePage(i, id, _) if sids.contains(id)                 =>
          // We need to effect to update the reference
          Effect(Future(SelectIdToDisplay(i, id)))

        case PreviewPage(i, id, st) if sids.contains(id)                 =>
          val isLoaded = s.view.loaded.values.toList.contains(id)
          // We need to effect to update the reference
          if (isLoaded) {
            Effect(Future(SelectIdToDisplay(i, id)))
          } else {
            Effect(Future(SelectSequencePreview(i, id, st)))
          }

        case SequenceConfigPage(i, id, st) if sids.contains(id)          =>
          // We need to effect to update the reference
          Effect(Future(ShowStepConfig(i, id, st)))

        case PreviewConfigPage(i, id, st) if sids.contains(id)           =>
          val isLoaded = s.view.loaded.values.toList.contains(id)
          // We need to effect to update the reference
          if (isLoaded) {
            Effect(Future(ShowStepConfig(i, id, st)))
          } else {
            Effect(Future(ShowPreviewStepConfig(i, id, st)))
          }

        case _                                                           =>
          // No matches
          VoidEffect
      }
      updated(value.copy(firstLoad = false), Effect(Future(CleanSequences)) >> effect)
    }
}
