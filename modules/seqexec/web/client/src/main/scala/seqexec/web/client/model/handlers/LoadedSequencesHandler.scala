// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.{ActionHandler, ActionResult, Effect, ModelRW, NoAction}
import seqexec.model.events._
import seqexec.web.client.model.SequencesOnDisplay
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Handles updates to the selected sequences set
 */
class LoadedSequencesHandler[M](modelRW: ModelRW[M, SequencesOnDisplay]) extends ActionHandler(modelRW) with Handlers {
  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(LoadSequenceUpdated(i, sid, view)) =>
      println("here")
      if (value.loadedIds =!= view.loaded.values.toList) {
      println("effect")
        effectOnly(Effect(Future(UpdateOnLoadUpdate(i, sid, view.loaded.values.toList))))
      } else {
        noChange
      }

    case ServerMessage(ClearLoadedSequencesUpdated(_)) =>
      updated(value.updateLoaded(Nil))

    case ServerMessage(s: SeqexecModelUpdate) =>
      // we need to send an effect for this to get the references to work correctly
      if (value.loadedIds =!= s.view.loaded.values.toList) {
        effectOnly(Effect(Future(UpdateLoadedSequences(s.view.loaded.values.toList))))
      } else {
        noChange
      }

    case UpdateLoadedSequences(loaded) =>
      // we need to send an effect for this to get the references to work correctly
      val refs = loaded.map(SeqexecCircuit.sequenceRef)
      updated(value.updateLoaded(refs.toList))

    case UpdateOnLoadUpdate(i, sid, ids) =>
      val refs = ids.map(SeqexecCircuit.sequenceRef)
      updated(value.unsetPreviewOn(sid).updateLoaded(refs.toList).focusOnSequence(i, sid))

    case LoadSequence(observer, i, id) =>
      val loadSequence = Effect(SeqexecWebClient.loadSequence(i, id, observer).map(_ => NoAction))
      effectOnly(loadSequence)
  }
}
