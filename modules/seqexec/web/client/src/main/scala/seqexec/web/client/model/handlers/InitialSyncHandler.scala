// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{Action, ActionHandler, ActionResult, Effect, ModelRW}
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

  def pageE(action: Action): Effect =
    PageActionP.getOption(action).map(p => Effect(Future(NavigateTo(p)))).getOrElse(VoidEffect)

  def handle: PartialFunction[Any, ActionResult[M]] = {
    // Otherwise, update the model to reflect the current page
    case ServerMessage(s: SeqexecModelUpdate) if value.firstLoad                                 =>
      // the page maybe not in sync with the tabs. Let's fix that
      val sids = s.view.queue.map(_.id)
      val loaded = s.view.loaded.values.toList
      val effect = value.location match {
        case p @ SequencePage(_, id, _) if loaded.contains(id)     =>
          // We need to effect to update the reference
          Effect(Future(PageActionP.reverseGet(p)))

        case SequencePage(_, _, _)                                 =>
          // An unkown page was shown
          val effect = loaded.headOption.flatMap { id =>
            s.view.queue.find(_.id === id).map { s =>
              val action = SelectIdToDisplay(s.metadata.instrument, id, 0)
              Effect(Future(action)) >> pageE(action)
            }
          }
          effect.getOrElse(VoidEffect)

        case p @ SequenceConfigPage(_, id, _) if sids.contains(id) =>
          // We need to effect to update the reference
          Effect(Future(PageActionP.reverseGet(p)))

        case p @ PreviewPage(i, id, st) if sids.contains(id)       =>
          val isLoaded = loaded.contains(id)
          // We need to effect to update the reference
          if (isLoaded) {
            val action = SelectIdToDisplay(i, id, st)
            Effect(Future(action)) >> pageE(action)
          } else {
            Effect(Future(PageActionP.reverseGet(p)))
          }

        case PreviewConfigPage(i, id, st) if sids.contains(id)     =>
          val isLoaded = loaded.contains(id)
          // We need to effect to update the reference
          if (isLoaded) {
            Effect(Future(ShowStepConfig(i, id, st)))
          } else {
            Effect(Future(ShowPreviewStepConfig(i, id, st)))
          }

        case _                                                     =>
          // No matches
          VoidEffect
      }
      updated(value.copy(firstLoad = false), Effect(Future(CleanSequences)) >> effect)
    }
}
