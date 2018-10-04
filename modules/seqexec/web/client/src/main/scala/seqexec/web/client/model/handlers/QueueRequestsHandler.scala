// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.model.SequencesQueue
import seqexec.model.SequenceView
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient

/**
  * Handles actions sending requests to the backend
  */
class QueueRequestsHandler[M](modelRW: ModelRW[M, SequencesQueue[SequenceView]])
    extends ActionHandler(modelRW)
    with Handlers[M, SequencesQueue[SequenceView]] {

  def handleAddAllDayCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestAllDayCal(qid) =>
      val ids = value.sessionQueue.map(_.id)
      effectOnly(
        requestEffect(qid,
                      SeqexecWebClient.addSequencesToQueue(ids),
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

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleAddAllDayCal, handleClearAllCal).combineAll

}
