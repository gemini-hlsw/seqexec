package edu.gemini.seqexec.web.client.model

import diode.data._
import diode.react.ReactConnector
import diode.util.RunAfterJS
import diode._
import edu.gemini.seqexec.model._
import edu.gemini.seqexec.web.client.model.SeqexecCircuit.SearchResults
import edu.gemini.seqexec.web.client.services.{Audio, SeqexecWebClient}
import edu.gemini.seqexec.web.common.{SeqexecQueue, Sequence}
import org.scalajs.dom._
import upickle.default._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scalaz.{-\/, \/, \/-}

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
      val logE = SeqexecCircuit.appendToLogE(s"Sequence ${s.id} added to the queue")
      val u = value.map(u => u.copy((s :: u.queue.filter(_.id != s.id)).reverse))
      updated(u, logE)
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
      val empty = value.map(l => l.size == 1 && l.exists(_.id == s.id)).getOrElse(false)
      // TODO, this should be an effect, but somehow it breaks the queue tracking
      if (empty) {
        SeqexecCircuit.dispatch(CloseSearchArea)
      }
      updated(value.map(_.filterNot(_ == s)))
  }
}

/**
  * Handles sequence execution actions
  */
class SequenceExecutionHandler[M](modelRW: ModelRW[M, Pot[SeqexecQueue]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[AnyRef, ActionResult[M]] = {
    case RequestRun(s) =>
      effectOnly(Effect(SeqexecWebClient.run(s).map(r => if (r.error) RunStartFailed(s) else RunStarted(s))))
    case RequestStop(s) =>
      effectOnly(Effect(SeqexecWebClient.stop(s).map(r => if (r.error) RunStopFailed(s) else RunStopped(s))))
    // We could react to these events but we rather wait for the command from the event queue
    case RunStarted(s) =>
      noChange
    case RunStartFailed(s) =>
      noChange
    case RunStopped(s) =>
      // Normally we'd like to wait for the event queue to send us a stop, but that isn't yet working, so this will do
      val logE = SeqexecCircuit.appendToLogE(s"Sequence ${s.id} aborted")
      updated(value.map(_.abortSequence(s.id)), logE)
    case RunStopFailed(s) =>
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
  * Handles actions related to the development console
  */
class LoginBoxHandler[M](modelRW: ModelRW[M, SectionVisibilityState]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[AnyRef, ActionResult[M]] = {
    case OpenLoginBox if value == SectionClosed =>
      updated(SectionOpen)
    case CloseLoginBox if value == SectionOpen  =>
      updated(SectionClosed)
  }
}

/**
  * Handles actions related to the changing the selection of the displayed sequence
  */
class SequenceDisplayHandler[M](modelRW: ModelRW[M, SequencesOnDisplay]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[AnyRef, ActionResult[M]] = {
    case SelectToDisplay(s) =>
      val ref = SeqexecCircuit.sequenceRef(s.id)
      updated(value.focusOnSequence(ref))
    case ShowStep(s, i) =>
      if (value.instrumentSequences.focus.sequence().exists(_.id == s.id)) {
        updated(value.showStep(i))
      } else {
        noChange
      }
    case UnShowStep(s) =>
      if (value.instrumentSequences.focus.sequence().exists(_.id == s.id)) {
        updated(value.unshowStep)
      } else {
        noChange
      }
  }
}

/**
  * Handles updates to the log
  */
class GlobalLogHandler[M](modelRW: ModelRW[M, GlobalLog]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[AnyRef, ActionResult[M]] = {
    case AppendToLog(s) =>
      updated(value.append(s))
  }
}

/**
  * Handles actions related to the changing the selection of the displayed sequence
  */
class WebSocketEventsHandler[M](modelRW: ModelRW[M, (Pot[SeqexecQueue], WebSocketsLog, Option[UserDetails])]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case NewSeqexecEvent(SeqexecConnectionOpenEvent(u)) =>
      updated(value.copy(_3 = u))
    case NewSeqexecEvent(event @ SequenceStartEvent(id)) =>
      val logE = SeqexecCircuit.appendToLogE(s"Sequence $id started")
      updated(value.copy(_1 = value._1.map(_.sequenceRunning(id)), _2 = value._2.append(event)), logE)

    case NewSeqexecEvent(event @ StepExecutedEvent(id, c, _, f)) =>
      val logE = SeqexecCircuit.appendToLogE(s"Sequence $id, step $c completed")
      updated(value.copy(_1 = value._1.map(_.markStepAsCompleted(id, c, f)), _2 = value._2.append(event)), logE)

    case NewSeqexecEvent(event @ SequenceCompletedEvent(id)) =>
      val audioEffect = Effect.action(new Audio("/sequencecomplete.mp3").play())
      val logE = SeqexecCircuit.appendToLogE(s"Sequence $id completed")
      updated(value.copy(_1 = value._1.map(_.sequenceCompleted(id)), _2 = value._2.append(event)), audioEffect >> logE)

    case NewSeqexecEvent(s) =>
      // Ignore unknown events
      noChange
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

  def appendToLogE(s: String) =
    Effect(Future(AppendToLog(s)))

  // TODO Make into its own class
  val webSocket = {
    import org.scalajs.dom.document

    val host = document.location.host

    def onOpen(e: Event): Unit = {
      dispatch(AppendToLog("Connected"))
      dispatch(ConnectionOpened)
    }

    def onMessage(e: MessageEvent): Unit = {
      \/.fromTryCatchNonFatal(read[SeqexecEvent](e.data.toString)) match {
        case \/-(event) => dispatch(NewSeqexecEvent(event))
        case -\/(t)     => println(s"Error decoding event ${t.getMessage}")
      }
    }

    def onError(e: ErrorEvent): Unit = {
      println("Error " + e)
      dispatch(ConnectionError(e.message))
    }

    def onClose(e: CloseEvent): Unit = {
      println("Close ")
      dispatch(ConnectionClosed)
    }

    val ws = new WebSocket(s"ws://$host/api/seqexec/events")
    ws.onopen = onOpen _
    ws.onmessage = onMessage _
    ws.onerror = onError _
    ws.onclose = onClose _
    Some(ws)
  }

  val queueHandler           = new QueueHandler(zoomRW(_.queue)((m, v) => m.copy(queue = v)))
  val searchHandler          = new SearchHandler(zoomRW(_.searchResults)((m, v) => m.copy(searchResults = v)))
  val searchAreaHandler      = new SearchAreaHandler(zoomRW(_.searchAreaState)((m, v) => m.copy(searchAreaState = v)))
  val devConsoleHandler      = new DevConsoleHandler(zoomRW(_.devConsoleState)((m, v) => m.copy(devConsoleState = v)))
  val loginBoxHandler        = new LoginBoxHandler(zoomRW(_.loginBox)((m, v) => m.copy(loginBox = v)))
  val wsLogHandler           = new WebSocketEventsHandler(zoomRW(m => (m.queue, m.webSocketLog, m.user))((m, v) => m.copy(queue = v._1, webSocketLog = v._2, user = v._3)))
  val sequenceDisplayHandler = new SequenceDisplayHandler(zoomRW(_.sequencesOnDisplay)((m, v) => m.copy(sequencesOnDisplay = v)))
  val sequenceExecHandler    = new SequenceExecutionHandler(zoomRW(_.queue)((m, v) => m.copy(queue = v)))
  val globalLogHandler       = new GlobalLogHandler(zoomRW(_.globalLog)((m, v) => m.copy(globalLog = v)))

  override protected def initialModel = SeqexecAppRootModel.initial

  // Reader for a specific sequence if available
  def sequenceReader(id: String):ModelR[_, Pot[Sequence]] =
    zoomFlatMap(_.queue)(_.queue.find(_.id == id).fold(Empty: Pot[Sequence])(s => Ready(s)))

  /**
    * Makes a reference to a sequence on the queue.
    * This way we have a normalized model and need to update it in only one place
    */
  def sequenceRef(id: String):RefTo[Pot[Sequence]] =
    RefTo(sequenceReader(id))

  override protected def actionHandler = composeHandlers(
    queueHandler,
    searchHandler,
    searchAreaHandler,
    devConsoleHandler,
    loginBoxHandler,
    wsLogHandler,
    sequenceDisplayHandler,
    globalLogHandler,
    sequenceExecHandler)
}
