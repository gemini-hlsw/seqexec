// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{ActionHandler, ActionResult, Effect, ModelRW}
import seqexec.web.client.ModelOps._
import seqexec.model.events._
import seqexec.web.client.model.Pages
import seqexec.web.client.model.Pages._
import seqexec.web.client.actions._
import cats.implicits._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Handles syncing added sequences to the page
 */
class SyncToAddedRemovedRunHandler[M](modelRW: ModelRW[M, Pages.SeqexecPages]) extends ActionHandler(modelRW) with Handlers {
  private def inInstrumentPage = value match {
    case Root | SoundTest => true
    case _                => false
  }

  def handleSyncPageToAddedSequence: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceLoaded(id, view)) =>
      val newSequence = view.queue.find(_.id === id)
      // If a new sequence is loaded then switch the page and focus
      // if we are on the instrument route
      val toSelect = for {
        s <- newSequence
        if inInstrumentPage
      } yield s
      toSelect match {
        case Some(seq) =>
          val instrument = seq.metadata.instrument
          val sid = seq.id
          val step = seq.progress.last
          // We need to use an effect here as the model is not fully resolved
          val effect = Effect(Future(SelectIdToDisplay(sid)))
          value match {
            case Root | SoundTest =>
              updated(SequencePage(instrument, id, step), effect)
            case _                                    =>
              effectOnly(effect)
          }
        case _ =>
          noChange
      }
    }

  def handleSyncPageToRemovedSequence: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceUnloaded(_, _)) =>
      // If the selecte id is removed, reset the route
      value match {
        case SequencePage(_, _, _) =>
          updated(Root)
        case _                     =>
          noChange
      }
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleSyncPageToRemovedSequence, handleSyncPageToAddedSequence).combineAll

}
