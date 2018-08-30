// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.{ActionHandler, ActionResult, Effect, ModelRW, NoAction}
import seqexec.model.events._
import seqexec.web.client.model.Pages.SequencePage
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient
import seqexec.web.client.circuit.SODLocationFocus
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Handles updates to the selected sequences set
 */
class LoadedSequencesHandler[M](modelRW: ModelRW[M, SODLocationFocus]) extends ActionHandler(modelRW) with Handlers[M, SODLocationFocus] {
  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(LoadSequenceUpdated(i, sid, view)) =>
      // Update selected and the page
      val upSelected = SODLocationFocus.sod.modify(_.updateFromQueue(view).unsetPreviewOn(sid).focusOnSequence(i, sid))
      val upLocation = SODLocationFocus.location.set(SequencePage(i, sid, 0))
      updatedL(upSelected >>> upLocation)

    case ServerMessage(s: SeqexecModelUpdate) =>
      updated(SODLocationFocus.sod.modify(_.updateFromQueue(s.view))(value))

    case LoadSequence(observer, i, id) =>
      val loadSequence = Effect(SeqexecWebClient.loadSequence(i, id, observer).map(_ => NoAction))
      effectOnly(loadSequence)
  }
}
