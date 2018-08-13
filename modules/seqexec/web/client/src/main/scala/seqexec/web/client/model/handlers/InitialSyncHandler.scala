// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{ActionHandler, ActionResult, RootModelR, Effect, ModelRW}
import diode.data.RefTo
import seqexec.model.SequenceView
import seqexec.model.events.SeqexecModelUpdate
import seqexec.web.client.actions._
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages._
import seqexec.web.client.ModelOps._
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
        // val seq = RefTo(new RootModelR(running))
        updated(value.copy(location = SequencePage(f.metadata.instrument, f.id, f.progress.last)))//, sod = value.sod.focusOnSequence(seq)))
      }

    // Otherwise, update the model to reflect the current page
    case ServerMessage(s: SeqexecModelUpdate) if value.firstLoad                                 =>
      // the page maybe not in sync with the tabs. Let's fix that
      val sids = s.view.queue.map(_.id)
      value.location match {
        case SequencePage(_, id, _) if sids.contains(id)                 =>
          // We are on a sequence page, update the model
          // val seq = RefTo(new RootModelR(s.view.queue.find(_.id === id)))
          // We need to effect to update the reference
          val effect = Effect(Future(SelectIdToDisplay(id)))
          updated(value.copy(/*sod = value.sod.focusOnSequence(seq), */firstLoad = false), effect)

        case SequenceConfigPage(_, id, _) if sids.contains(id)           =>
          // We are on a seq config page, update the model
          // val seq = RefTo(new RootModelR(s.view.queue.find(_.id === id)))
          // val effect = Effect(Future(ShowStepConfig(id, step, isPreview = false)))
          // updated(value.copy(sod = value.sod.focusOnSequence(seq).showStepConfig(step - 1), firstLoad = false), effect)
          noChange

        case PreviewConfigPage(_, id, step) if sids.contains(id)           =>
          // We are on a seq config page, update the model
          val seq = RefTo(new RootModelR(s.view.queue.find(_.id === id)))
          val effect = Effect(Future(ShowStepConfig(id, step, isPreview = true)))
          updated(value.copy(sod = value.sod.previewSequence(seq).showStepConfig(step - 1), firstLoad = false), effect)
        case _                                                              =>
          // No matches
          updated(value.copy(firstLoad = false))
      }
    }
}
