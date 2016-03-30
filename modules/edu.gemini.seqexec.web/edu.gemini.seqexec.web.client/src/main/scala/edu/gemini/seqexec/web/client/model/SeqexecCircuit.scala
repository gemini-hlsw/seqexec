package edu.gemini.seqexec.web.client.model

import diode.data.{Empty, Pot, Ready}
import diode.react.ReactConnector
import diode.{ActionHandler, Circuit, Effect, ModelRW}
import edu.gemini.seqexec.web.client.services.SeqexecWebClient
import edu.gemini.seqexec.web.common.SeqexecQueue

import scala.concurrent.ExecutionContext.Implicits.global

// Actions

// Request loading the queue
case object RefreshQueue
case class UpdatedQueue(queue: SeqexecQueue)

/**
  * Handles actions related to todos
  *
  * @param modelRW Reader/Writer to access the model
  */
class QueueHandler[M](modelRW: ModelRW[M, Pot[SeqexecQueue]]) extends ActionHandler(modelRW) {
  override def handle = {
    case RefreshQueue        =>
      // Call the backend requesting the queue
      effectOnly(Effect(SeqexecWebClient.readQueue().map(UpdatedQueue)))
    case UpdatedQueue(queue) =>
      updated(Ready(queue))
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
