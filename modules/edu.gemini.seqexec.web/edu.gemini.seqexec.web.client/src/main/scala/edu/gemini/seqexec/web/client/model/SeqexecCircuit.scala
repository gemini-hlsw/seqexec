package edu.gemini.seqexec.web.client.model

import diode.data.{Empty, Pot}
import diode.react.ReactConnector
import diode.{ActionHandler, Circuit, ModelRW}
import edu.gemini.seqexec.web.common.SeqexecQueue

case class UpdateQueue(queue: Pot[SeqexecQueue])

/**
  * Handles actions related to todos
  *
  * @param modelRW Reader/Writer to access the model
  */
class QueueHandler[M](modelRW: ModelRW[M, Pot[SeqexecQueue]]) extends ActionHandler(modelRW) {
  override def handle = {
    case UpdateQueue(queue) => updated(queue)
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
