package edu.gemini.seqexec.web.client.model

import java.util.logging.{Level, Logger}

import diode.data._
import diode.react.ReactConnector
import diode.util.RunAfterJS
import diode._
import edu.gemini.seqexec.model.{NewBooPicklers, UserDetails}
import edu.gemini.seqexec.model.SharedModel.{SeqexecEvent, SequenceView}
import edu.gemini.seqexec.web.client.model.SeqexecCircuit.SearchResults
import edu.gemini.seqexec.web.client.services.log.ConsoleHandler
import edu.gemini.seqexec.web.client.services.SeqexecWebClient
import edu.gemini.seqexec.web.common.LogMessage._
import org.scalajs.dom._
import boopickle.Default._
import edu.gemini.seqexec.model.SharedModel.SeqexecEvent.{ConnectionOpenEvent, SequenceLoaded}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scalaz.{-\/, \/, \/-}

/**
  * Handles actions related to search
  */
class LoadHandler[M](modelRW: ModelRW[M, Pot[SeqexecCircuit.SearchResults]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case action: LoadSequence =>
      // Request loading the queue with ajax
      val loadEffect = action.effect(SeqexecWebClient.read(action.criteria))(identity)
      action.handleWith(this, loadEffect)(PotAction.handler(250.milli))
  }
}

/**
  * Handles sequence execution actions
  */
class SequenceExecutionHandler[M](modelRW: ModelRW[M, List[SequenceView]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[Any, ActionResult[M]] = {
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
      noChange

    case RunStopFailed(s) =>
      noChange
  }
}

/**
  * Handles actions related to the search area, used to open/close the area
  */
class SearchAreaHandler[M](modelRW: ModelRW[M, SectionVisibilityState]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[Any, ActionResult[M]] = {
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

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ToggleDevConsole if value == SectionOpen   =>
      updated(SectionClosed)

    case ToggleDevConsole if value == SectionClosed =>
      updated(SectionOpen)
  }
}

/**
  * Handles actions related to opening/closing the login box
  */
class LoginBoxHandler[M](modelRW: ModelRW[M, SectionVisibilityState]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case OpenLoginBox if value == SectionClosed =>
      updated(SectionOpen)

    case OpenLoginBox                           =>
      noChange

    case CloseLoginBox if value == SectionOpen  =>
      updated(SectionClosed)

    case CloseLoginBox                          =>
      noChange
  }
}

/**
  * Handles actions related to opening/closing the login box
  */
class UserLoginHandler[M](modelRW: ModelRW[M, Option[UserDetails]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case LoggedIn(u) =>
      // Close the login box
      val effect = Effect(Future(CloseLoginBox))
      updated(Some(u), effect)

    case Logout =>
      val effect = Effect(SeqexecWebClient.logout().map(_ => NoAction))
      // Remove the user and call logout
      updated(None, effect)
  }
}

/**
  * Handles actions related to the changing the selection of the displayed sequence
  */
class SequenceDisplayHandler[M](modelRW: ModelRW[M, SequencesOnDisplay]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[Any, ActionResult[M]] = {
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

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case AppendToLog(s) =>
      updated(value.append(s))
  }
}

/**
  * Handles the WebSocket connection and performs reconnection if needed
  */
class WebSocketHandler[M](modelRW: ModelRW[M, WebSocketConnection]) extends ActionHandler(modelRW) with NewBooPicklers {
  // Import explicitly the custom pickler
  import SeqexecEvent._

  implicit val runner = new RunAfterJS

  val logger = Logger.getLogger(this.getClass.getSimpleName)
  // Reconfigure to avoid sending ajax events in this logger
  logger.setUseParentHandlers(false)
  logger.addHandler(new ConsoleHandler(Level.FINE))

  // Makes a websocket connection and setups event listeners
  def webSocket(nextDelay: Int) = Future[Action] {
    import org.scalajs.dom.document

    val host = document.location.host
    val url = s"ws://$host/api/seqexec/events"

    val ws = new WebSocket(url)

    def onOpen(e: Event): Unit = {
      logger.info(s"Connected to $url")
      SeqexecCircuit.dispatch(Connected(ws, 0))
    }

    def onMessage(e: MessageEvent): Unit = {
      val byteBuffer = TypedArrayBuffer.wrap(e.data.asInstanceOf[ArrayBuffer])
      \/.fromTryCatchNonFatal(Unpickle[SeqexecEvent].fromBytes(byteBuffer)) match {
        case \/-(event) => println(s"Decoding event: $event"); SeqexecCircuit.dispatch(NewSeqexecEvent(event))
        case -\/(t)     => println(s"Error decoding event ${t.getMessage}")
      }
    }

    def onError(e: ErrorEvent): Unit = {
      // Unfortunately reading the event is not cross-browser safe
      logger.severe("Error on websocket")
    }

    def onClose(e: CloseEvent): Unit =
      // Increase the delay to get exponential backoff with a minimum of 200ms and a max of 1m
      SeqexecCircuit.dispatch(ConnectionClosed(math.min(60000, math.max(200, nextDelay * 2))))

    ws.binaryType = "arraybuffer"
    ws.onopen = onOpen _
    ws.onmessage = onMessage _
    ws.onerror = onError _
    ws.onclose = onClose _
    Connecting
  }.recover {
    case _: Throwable => NoAction
  }

  // This is essentially a state machine to handle the connection status and
  // can reconnect if needed
  override protected def handle = {
    case WSConnect(d) =>
      effectOnly(Effect(webSocket(d)).after(d.millis))

    case Connecting =>
      noChange

    case Connected(ws, delay) =>
      val effect = Effect.action(AppendToLog("Connected"))
      updated(WebSocketConnection(Ready(ws), delay), effect)

    case ConnectionError(e) =>
      effectOnly(Effect.action(AppendToLog(e)))

    case ConnectionClosed(d) =>
      val next = math.min(60000, math.max(250, value.nextAttempt * 2))
      logger.fine("Retry connecting in "+ next)
      val effect = Effect(Future(WSConnect(next)))
      updated(WebSocketConnection(Pending(), next), effect)
  }
}

/**
  * Handles messages received over the WS channel
  */
class WebSocketEventsHandler[M](modelRW: ModelRW[M, (List[SequenceView], WebSocketsLog, Option[UserDetails])]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case NewSeqexecEvent(ConnectionOpenEvent(u)) =>
      updated(value.copy(_3 = u))

    case NewSeqexecEvent(SequenceLoaded(sv)) =>
      val logE = SeqexecCircuit.appendToLogE(s"Sequence loaded")
      updated(value.copy(_1 = sv), logE)

    /*case NewSeqexecEvent(event @ SequenceStartEvent(id)) =>
      val logE = SeqexecCircuit.appendToLogE(s"Sequence $id started")
      updated(value.copy(_1 = value._1.map(_.sequenceRunning(id)), _2 = value._2.append(event)), logE)

    case NewSeqexecEvent(event @ StepExecutedEvent(id, c, _, f)) =>
      val logE = SeqexecCircuit.appendToLogE(s"Sequence $id, step $c completed")
      updated(value.copy(_1 = value._1.map(_.markStepAsCompleted(id, c, f)), _2 = value._2.append(event)), logE)

    case NewSeqexecEvent(event @ SequenceCompletedEvent(id)) =>
      val audioEffect = Effect(Future(new Audio("/sequencecomplete.mp3").play()).map(_ => NoAction))
      val logE = SeqexecCircuit.appendToLogE(s"Sequence $id completed")
      updated(value.copy(_1 = value._1.map(_.sequenceCompleted(id)), _2 = value._2.append(event)), audioEffect >> logE)*/

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

  val seqexecQueueEq = potStateEq[List[SequenceView]]
  val searchResultsEq = potStateEq[SearchResults]
  val sequenceEq = potStateEq[SequenceView]
}

/**
  * Utility class to let components more easily switch parts of the UI depending on the context
  */
case class ClientStatus(u: Option[UserDetails], w: WebSocketConnection) {
  def isLogged = u.isDefined
  def isConnected = w.ws.isReady
}

/**
  * Contains the model for Diode
  */
object SeqexecCircuit extends Circuit[SeqexecAppRootModel] with ReactConnector[SeqexecAppRootModel] {
  type SearchResults = List[SequenceView]

  val logger = Logger.getLogger(SeqexecCircuit.getClass.getSimpleName)

  def appendToLogE(s: String) =
    Effect(Future(AppendToLog(s)))

  val wsHandler              = new WebSocketHandler(zoomRW(_.ws)((m, v) => m.copy(ws = v)))
  val searchHandler          = new LoadHandler(zoomRW(_.searchResults)((m, v) => m.copy(searchResults = v)))
  val searchAreaHandler      = new SearchAreaHandler(zoomRW(_.searchAreaState)((m, v) => m.copy(searchAreaState = v)))
  val devConsoleHandler      = new DevConsoleHandler(zoomRW(_.devConsoleState)((m, v) => m.copy(devConsoleState = v)))
  val loginBoxHandler        = new LoginBoxHandler(zoomRW(_.loginBox)((m, v) => m.copy(loginBox = v)))
  val userLoginHandler       = new UserLoginHandler(zoomRW(_.user)((m, v) => m.copy(user = v)))
  val wsLogHandler           = new WebSocketEventsHandler(zoomRW(m => (m.sequences, m.webSocketLog, m.user))((m, v) => m.copy(sequences = v._1, webSocketLog = v._2, user = v._3)))
  val sequenceDisplayHandler = new SequenceDisplayHandler(zoomRW(_.sequencesOnDisplay)((m, v) => m.copy(sequencesOnDisplay = v)))
  val sequenceExecHandler    = new SequenceExecutionHandler(zoomRW(_.sequences)((m, v) => m.copy(sequences = v)))
  val globalLogHandler       = new GlobalLogHandler(zoomRW(_.globalLog)((m, v) => m.copy(globalLog = v)))

  override protected def initialModel = SeqexecAppRootModel.initial

  // Some useful readers

  // Reader for a specific sequence if available
  def sequenceReader(id: String):ModelR[_, Option[SequenceView]] =
    zoom(_.sequences.find(_.id == id))//.fold(Empty: Pot[Sequence])(s => Ready(s)))

  // Reader to indicate the allowed interactions
  def status: ModelR[SeqexecAppRootModel, ClientStatus] = zoom(m => ClientStatus(m.user, m.ws))

  // Reader for search results
  val searchResults: ModelR[SeqexecAppRootModel, Pot[List[SequenceView]]] = zoom(_.searchResults)

  // Reader for sequences on display
  val sequencesOnDisplay: ModelR[SeqexecAppRootModel, SequencesOnDisplay] = zoom(_.sequencesOnDisplay)

  val statusAndSearchResults = SeqexecCircuit.status.zip(SeqexecCircuit.searchResults)
  val statusAndSequences = SeqexecCircuit.status.zip(SeqexecCircuit.sequencesOnDisplay)

  /**
    * Makes a reference to a sequence on the queue.
    * This way we have a normalized model and need to update it in only one place
    */
  def sequenceRef(id: String): RefTo[Option[SequenceView]] =
    RefTo(sequenceReader(id))

  override protected def actionHandler = composeHandlers(
    wsHandler,
    searchHandler,
    searchAreaHandler,
    devConsoleHandler,
    loginBoxHandler,
    userLoginHandler,
    wsLogHandler,
    sequenceDisplayHandler,
    globalLogHandler,
    sequenceExecHandler)

  /**
    * Handles a fatal error most likely during action processing
    */
  override def handleFatal(action: Any, e: Throwable): Unit = {
    logger.severe(s"Action not handled $action")
    super.handleFatal(action, e)
  }

  /**
    * Handle a non-fatal error, such as dispatching an action with no action handler.
    */
  override def handleError(msg: String): Unit = {
    logger.severe(s"Action error $msg")
    throw new Exception(s"handleError called with: $msg")
  }

}
