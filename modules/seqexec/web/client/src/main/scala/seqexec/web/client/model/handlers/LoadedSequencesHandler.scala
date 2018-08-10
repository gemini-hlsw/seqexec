// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{ActionHandler, ActionResult, Effect, ModelRW, NoAction}
import seqexec.model.events._
import seqexec.web.client.model.SequencesOnDisplay
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Handles updates to the selected sequences set
 */
class LoadedSequencesHandler[M](modelRW: ModelRW[M, SequencesOnDisplay]) extends ActionHandler(modelRW) with Handlers {
  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(LoadSequenceUpdated(i, sid)) =>
      println((i, sid))
      updated(value.unsetPreview)

    case LoadSequence(i, id) =>
      effectOnly(Effect(SeqexecWebClient.loadSequence(i, id).map(r => if (r.error) NoAction else NoAction)))
  }
}
