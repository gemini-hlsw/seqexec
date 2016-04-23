package edu.gemini.seqexec.web.client.model

import diode.data.{Pot, PotAction}
import diode.react.ReactConnector
import diode.util.RunAfterJS
import diode._
import edu.gemini.seqexec.model.{SeqexecEvent, SequenceStartEvent}
import edu.gemini.seqexec.web.client.model.SeqexecCircuit.SearchResults
import edu.gemini.seqexec.web.client.services.SeqexecWebClient
import edu.gemini.seqexec.web.common.{SeqexecQueue, Sequence}
import org.scalajs.dom._
import upickle.default._

import scala.concurrent.duration._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// Action Handlers
/**
  * Handles actions related to the queue like loading and adding new elements
  */
class QueueHandler[M](modelRW: ModelRW[M, Pot[SeqexecQueue]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[AnyRef, ActionResult[M]] = {
    case action: UpdatedQueue =>
      // Request loading the queue with ajax
      val loadEffect = action.effect(SeqexecWebClient.readQueue())(identity)
      action.handleWith(this, loadEffect)(PotAction.handler(250.milli))
    case AddToQueue(s) =>
      // Append to the current queue if not in the queue already
      val u = value.map(u => u.copy((s :: u.queue.filter(_.id != s.id)).reverse))
      updated(u)
  }
}

/**
  * Handles actions related to search
  */
class SearchHandler[M](modelRW: ModelRW[M, Pot[SeqexecCircuit.SearchResults]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[AnyRef, ActionResult[M]] = {
    case action: SearchSequence =>
      // Request loading the queue with ajax
      val loadEffect = action.effect(SeqexecWebClient.read(action.criteria))(identity)
      action.handleWith(this, loadEffect)(PotAction.handler(250.milli))
    case RemoveFromSearch(s) =>
      updated(value.map(_.filterNot(_ == s)))
  }
}

/**
  * Handles sequence execution actions
  */
class SequenceExecutionHandler[M](modelRW: ModelRW[M, Pot[SeqexecQueue]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case RequestRun(s) =>
      effectOnly(Effect(SeqexecWebClient.run(s).map(r => if (r.error) RunStartFailed(s) else RunStarted(s))))
    case RunStarted(s) =>
      // We could react to this change but we rather wait for the command from the event queue
      noChange
    case RunStartFailed(s) =>
      noChange
  }
}

/**
  * Handles actions related to the search area, used to open/close the area
  */
class SearchAreaHandler[M](modelRW: ModelRW[M, SectionVisibilityState]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[AnyRef, ActionResult[M]] = {
    case OpenSearchArea  =>
      updated(SectionOpen)
    case CloseSearchArea =>
      updated(SectionClosed)
  }
}

/**
  * Handles actions related to the development console
  */
class DevConsoleHandler[M](modelRW: ModelRW[M, SectionVisibilityState]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[AnyRef, ActionResult[M]] = {
    case ToggleDevConsole if value == SectionOpen   =>
      updated(SectionClosed)
    case ToggleDevConsole if value == SectionClosed =>
      updated(SectionOpen)
  }
}

/**
  * Handles actions related to the changing the selection of the displayed sequence
  */
class SequenceDisplayHandler[M](modelRW: ModelRW[M, SequencesOnDisplay]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[AnyRef, ActionResult[M]] = {
    case SelectToDisplay(s) =>
      updated(value.sequenceForInstrument(s))
  }
}

/**
  * Handles actions related to the changing the selection of the displayed sequence
  */
class WebSocketEventsHandler[M](modelRW: ModelRW[M, (Pot[SeqexecQueue], WebSocketsLog)]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case NewMessage(s) =>
      val event = read[SeqexecEvent](s)
      val updatedQueue = event match {
        case SequenceStartEvent(id) => value._1.map(_.markAsRunning(id))
        case _                      => value._1
      }
      updated(value.copy(_1 = updatedQueue, _2 = value._2.append(event)))
  }
}

/**
  * Generates Eq comparisons for Pot[A], it is useful for state indicators
  */
object PotEq {
  def potStateEq[A]: FastEq[Pot[A]] = new FastEq[Pot[A]] {
    override def eqv(a: Pot[A], b: Pot[A]): Boolean = a.state == b.state
  }

  val seqexecQueueEq = potStateEq[SeqexecQueue]
  val searchResultsEq = potStateEq[SearchResults]
}

/**
  * Contains the model for Diode
  */
object SeqexecCircuit extends Circuit[SeqexecAppRootModel] with ReactConnector[SeqexecAppRootModel] {
  type SearchResults = List[Sequence]

  // TODO Make this into its own class and handle reconnections
  val webSocket = {
    import org.scalajs.dom.document

    val host = document.location.host

    def onopen(e: Event): Unit =
      dispatch(ConnectionOpened)

    def onmessage(e: MessageEvent): Unit =
      dispatch(NewMessage(e.data.toString))

    def onerror(e: ErrorEvent): Unit =
      dispatch(ConnectionError(e.message))

    def onclose(e: CloseEvent): Unit =
      dispatch(ConnectionClosed)

    val ws = new WebSocket(s"ws://$host/api/seqexec/events")
    ws.onopen = onopen _
    ws.onmessage = onmessage _
    ws.onerror = onerror _
    ws.onclose = onclose _
    Some(ws)
  }

  val queueHandler           = new QueueHandler(zoomRW(_.queue)((m, v) => m.copy(queue = v)))
  val searchHandler          = new SearchHandler(zoomRW(_.searchResults)((m, v) => m.copy(searchResults = v)))
  val searchAreaHandler      = new SearchAreaHandler(zoomRW(_.searchAreaState)((m, v) => m.copy(searchAreaState = v)))
  val devConsoleHandler      = new DevConsoleHandler(zoomRW(_.devConsoleState)((m, v) => m.copy(devConsoleState = v)))
  val wsLogHandler           = new WebSocketEventsHandler(zoomRW(m => (m.queue, m.webSocketLog))((m, v) => m.copy(queue = v._1, webSocketLog = v._2)))
  val sequenceDisplayHandler = new SequenceDisplayHandler(zoomRW(_.sequencesOnDisplay)((m, v) => m.copy(sequencesOnDisplay = v)))
  val sequenceExecHandler    = new SequenceExecutionHandler(zoomRW(_.queue)((m, v) => m.copy(queue = v)))

  override protected def initialModel = SeqexecAppRootModel.initial

  override protected def actionHandler = composeHandlers(queueHandler,
    searchHandler,
    searchAreaHandler,
    devConsoleHandler,
    wsLogHandler,
    sequenceDisplayHandler,
    sequenceExecHandler)
}
