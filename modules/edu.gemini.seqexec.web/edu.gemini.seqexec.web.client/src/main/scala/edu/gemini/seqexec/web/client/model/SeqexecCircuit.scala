package edu.gemini.seqexec.web.client.model

import diode.data.PotState._
import diode.data.{Empty, Pot, PotAction, PotState}
import diode.react.ReactConnector
import diode.util.{RunAfter, RunAfterJS}
import diode._
import edu.gemini.seqexec.web.client.services.SeqexecWebClient
import edu.gemini.seqexec.web.common.SeqexecQueue

import scala.concurrent.duration._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// Actions

// Request loading the queue
case class UpdatedQueue(potResult: Pot[SeqexecQueue]) extends PotAction[SeqexecQueue, UpdatedQueue] {
  override def next(newResult: Pot[SeqexecQueue]) = {
    UpdatedQueue(newResult)
  }
}

/**
  * Handles actions related to todos
  *
  * @param modelRW Reader/Writer to access the model
  */
class QueueHandler[M](modelRW: ModelRW[M, Pot[SeqexecQueue]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case action: UpdatedQueue =>
      val loadEffect = action.effect(SeqexecWebClient.readQueue())(identity)
      action.handleWith(this, loadEffect)(PotAction.handler(100.milli))
  }
}

/**
  * Root of the UI Model of the application
  */
case class SeqexecAppRootModel(queue: Pot[SeqexecQueue])

/**
  * Contains the model for Diode
  */
object SeqexecCircuit extends Circuit[SeqexecAppRootModel] with ReactConnector[SeqexecAppRootModel] {

  val queueHandler = new QueueHandler(zoomRW(_.queue)((m, v) => m.copy(queue = v)))

  override protected def initialModel = SeqexecAppRootModel(Empty)

  override protected def actionHandler = combineHandlers(queueHandler)
}
