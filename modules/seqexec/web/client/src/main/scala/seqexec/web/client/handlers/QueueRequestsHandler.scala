// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.model.ClientId
import seqexec.model.QueueId
import seqexec.web.client.actions._
import seqexec.web.client.circuit.QueueRequestsFocus
import seqexec.web.client.services.SeqexecWebClient

/**
  * Handles actions sending requests to the backend
  *
  * Note this handler is based on an unsafe lens. Don't change the model from here
  */
class QueueRequestsHandler[M](modelRW: ModelRW[M, QueueRequestsFocus])
    extends ActionHandler(modelRW)
    with Handlers[M, QueueRequestsFocus] {

  def handleAddAllDayCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestAllSelectedSequences(qid) =>
      val ids = value.seqFilter.filterS(value.sequences.sessionQueue).map(_.id)
      effectOnly(
        requestEffect(qid,
                      SeqexecWebClient.addSequencesToQueue(ids),
                      AllDayCalCompleted.apply,
                      AllDayCalFailed.apply))
  }

  def handleAddDayCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestAddSeqCal(qid, oid) =>
      effectOnly(
        requestEffect(qid,
                      SeqexecWebClient.addSequenceToQueue(oid),
                      AllDayCalCompleted.apply,
                      AllDayCalFailed.apply))
  }

  def handleClearAllCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestClearAllCal(qid) =>
      effectOnly(
        requestEffect(qid,
                      SeqexecWebClient.clearQueue,
                      ClearAllCalCompleted.apply,
                      ClearAllCalFailed.apply))
  }

  def handleRunCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestRunCal(qid) =>
      (value.clientId,
       value.queuesObserver.get(qid).orElse(value.calTabObserver))
        .mapN { (cid, obs) =>
          effectOnly(
            requestEffect2(
              (qid, cid),
              SeqexecWebClient.runQueue(_: QueueId, _: ClientId, obs),
              RunCalCompleted.apply,
              RunCalFailed.apply))
        }
        .getOrElse(noChange)
  }

  def handleStopCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestStopCal(qid) =>
      value.clientId
        .map { cid =>
          effectOnly(
            requestEffect2((qid, cid),
                           SeqexecWebClient.stopQueue,
                           StopCalCompleted.apply,
                           StopCalFailed.apply))
        }
        .getOrElse(noChange)
  }

  def handleRemoveSeqCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestRemoveSeqCal(qid, id) =>
      value.clientId
        .map { _ =>
          effectOnly(
            requestEffect2((qid, id),
                           SeqexecWebClient.removeSequenceFromQueue,
                           RemoveSeqCalCompleted.apply,
                           RemoveSeqCalFailed.apply(_: QueueId, id)))
        }
        .getOrElse(noChange)
  }

  def handleMoveCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestMoveCal(qid, oid, i) =>
      value.clientId
        .map { cid =>
          effectOnly(
            requestEffect2((qid, cid),
                           SeqexecWebClient.moveSequenceQueue(_: QueueId,
                                                              oid,
                                                              i,
                                                              _: ClientId),
                           MoveCalCompleted.apply,
                           MoveCalFailed.apply(_: QueueId, oid)))
        }
        .getOrElse(noChange)
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleAddAllDayCal,
         handleAddDayCal,
         handleClearAllCal,
         handleRunCal,
         handleStopCal,
         handleMoveCal,
         handleRemoveSeqCal).combineAll

}
