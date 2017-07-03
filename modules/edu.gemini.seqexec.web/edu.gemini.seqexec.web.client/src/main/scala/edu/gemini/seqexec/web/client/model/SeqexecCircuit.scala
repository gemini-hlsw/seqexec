package edu.gemini.seqexec.web.client.model

import java.util.logging.{Level, Logger}

import diode.data._
import diode.react.ReactConnector
import diode.util.RunAfterJS
import diode._
import edu.gemini.seqexec.model.{ModelBooPicklers, UserDetails}
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.web.client.model.SeqexecAppRootModel.LoadedSequences
import edu.gemini.seqexec.web.client.model.Pages._
import edu.gemini.seqexec.model.Model.SeqexecEvent.{ConnectionOpenEvent, SequenceCompleted}
import edu.gemini.seqexec.web.client.model.SeqexecCircuit.SearchResults
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.services.log.ConsoleHandler
import edu.gemini.seqexec.web.client.services.{SeqexecWebClient, Audio}
import org.scalajs.dom._
import boopickle.Default._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scalaz._
import Scalaz._

class NavigationHandler[M](modelRW: ModelRW[M, Pages.SeqexecPages]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  def handle: PartialFunction[Any, ActionResult[M]] = {
    case NavigateTo(page) =>
      updated(page)

    case NavigateSilentTo(page) =>
      val effect = page match {
        case InstrumentPage(i, None)     => Effect(Future(SelectInstrumentToDisplay(i)))
        case InstrumentPage(i, Some(id)) => Effect(Future(SelectIdToDisplay(i, id)))
        case _                           => Effect(Future(NoAction: Action))
      }
      updatedSilent(page, effect)

    case SyncToPage(s) =>
      // the page maybe not in sync with the tabs. Let's fix that
      value match {
        case InstrumentPage(i, Some(id)) if i === s.metadata.instrument && id === s.id =>
          effectOnly(Effect(Future(SelectToDisplay(s))))
        case _ =>
          noChange
      }

    case _ =>
      noChange
  }
}

/**
  * Handles sequence execution actions
  */
class SequenceExecutionHandler[M](modelRW: ModelRW[M, LoadedSequences]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case RequestRun(s) =>
      effectOnly(Effect(SeqexecWebClient.run(s).map(r => if (r.error) RunStartFailed(s) else RunStarted(s))))

    case RequestSync(s) =>
      effectOnly(Effect(SeqexecWebClient.sync(s).map(r => if (r.queue.isEmpty) RunSyncFailed(s) else RunSync(s))))

    case RequestPause(s) =>
      effectOnly(Effect(SeqexecWebClient.stop(s).map(r => if (r.error) RunPauseFailed(s) else RunPaused(s))))

    // We could react to these events but we rather wait for the command from the event queue
    case RunStarted(_) =>
      noChange

    case RunStartFailed(_) =>
      noChange

    case RunPaused(_) =>
      // Normally we'd like to wait for the event queue to send us a stop, but that isn't yet working, so this will do
      noChange

    case RunPauseFailed(_) =>
      noChange

    case UpdateObserver(sequence, name) =>
      val updateObserverE = Effect(SeqexecWebClient.setObserver(sequence, name).map(_ => NoAction))
      val updatedSequences = value.copy(queue = value.queue.collect {
        case s if s.metadata.instrument === sequence.metadata.instrument =>
          sequence.copy(metadata = s.metadata.copy(observer = Some(name)))
        case s                  => s
      })
      updated(updatedSequences, updateObserverE)

    case FlipSkipStep(sequence, step) =>
      updated(value.copy(queue = value.queue.collect {
        case s if s == sequence => sequence.flipStep(step)
        case s                  => s
      }))

    case FlipBreakpointStep(sequence, step) =>
      val breakpointRequest = Effect(SeqexecWebClient.breakpoint(sequence, step.flipBreakpoint).map(_ => NoAction))
      updated(value.copy(queue = value.queue.collect {
        case s if s == sequence => sequence.flipBreakpointAtStep(step)
        case s                  => s
      }), breakpointRequest)
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
    case SelectInstrumentToDisplay(i) =>
      updated(value.focusOnInstrument(i))

    case SelectIdToDisplay(i, id) =>
      updated(value.focusOnId(i, id))

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
 * Handles updates to the operator
 */
class OperatorHandler[M](modelRW: ModelRW[M, Option[Operator]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateOperator(name) =>
      val updateOperatorE = Effect(SeqexecWebClient.setOperator(name).map(_ => NoAction))
      updated(name.some, updateOperatorE)
  }
}

/**
 * Handles updates to conditions
 */
class ConditionsHandler[M](modelRW: ModelRW[M, Conditions]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateImageQuality(iq) =>
      val updateE = Effect(SeqexecWebClient.setImageQuality(iq).map(_ => NoAction))
      val updatedSequences = value.copy(iq = iq)
      updated(updatedSequences, updateE)

    case UpdateCloudCover(cc) =>
      val updateE = Effect(SeqexecWebClient.setCloudCover(cc).map(_ => NoAction))
      val updatedSequences = value.copy(cc = cc)
      updated(updatedSequences, updateE)

    case UpdateSkyBackground(sb) =>
      val updateE = Effect(SeqexecWebClient.setSkyBackground(sb).map(_ => NoAction))
      val updatedSequences = value.copy(sb = sb)
      updated(updatedSequences, updateE)

    case UpdateWaterVapor(wv) =>
      val updateE = Effect(SeqexecWebClient.setWaterVapor(wv).map(_ => NoAction))
      val updatedSequences = value.copy(wv = wv)
      updated(updatedSequences, updateE)

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
class WebSocketHandler[M](modelRW: ModelRW[M, WebSocketConnection]) extends ActionHandler(modelRW) with ModelBooPicklers {
  // Import explicitly the custom pickler
  implicit val runner = new RunAfterJS

  private val logger = Logger.getLogger(this.getClass.getSimpleName)
  // Reconfigure to avoid sending ajax events in this logger
  logger.setUseParentHandlers(false)
  logger.addHandler(new ConsoleHandler(Level.FINE))

  // Makes a websocket connection and setups event listeners
  def webSocket(nextDelay: Int): Future[Action] = Future[Action] {
    import org.scalajs.dom.document

    val host = document.location.host
    val protocol = document.location.protocol.startsWith("https") ? "wss" | "ws"
    val url = s"$protocol://$host/api/seqexec/events"

    val ws = new WebSocket(url)

    def onOpen(e: Event): Unit = {
      logger.info(s"Connected to $url")
      SeqexecCircuit.dispatch(Connected(ws, 0))
    }

    def onMessage(e: MessageEvent): Unit = {
      val byteBuffer = TypedArrayBuffer.wrap(e.data.asInstanceOf[ArrayBuffer])
      \/.fromTryCatchNonFatal(Unpickle[SeqexecEvent].fromBytes(byteBuffer)) match {
        case \/-(event) => println(s"Decoding event: ${event.getClass}"); SeqexecCircuit.dispatch(ServerMessage(event))
        case -\/(t)     => println(s"Error decoding event ${t.getMessage}")
      }
    }

    def onError(e: ErrorEvent): Unit =
      // Unfortunately reading the event is not cross-browser safe
      logger.severe("Error on websocket")

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
      // After connected to the Websocket request a refresh
      val refreshRequest = Effect(SeqexecWebClient.refresh().map(_ => NoAction))
      updated(WebSocketConnection(Ready(ws), delay), refreshRequest)

    case ConnectionError(e) =>
      effectOnly(Effect.action(AppendToLog(e)))

    case ConnectionClosed(_) =>
      val next = math.min(60000, math.max(250, value.nextAttempt * 2))
      logger.fine("Retry connecting in "+ next)
      val effect = Effect(Future(WSConnect(next)))
      updated(WebSocketConnection(Pending(), next), effect)
  }
}

/**
  * Handles messages received over the WS channel
  */
class WebSocketEventsHandler[M](modelRW: ModelRW[M, (LoadedSequences, Option[UserDetails])]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case ServerMessage(ConnectionOpenEvent(u)) =>
      updated(value.copy(_2 = u))

    case ServerMessage(SequenceCompleted(sv)) =>
      // Play audio when the sequence completes
      val audioEffect = Effect(Future(new Audio("/sequencecomplete.mp3").play()).map(_ => NoAction))
      updated(value.copy(_1 = sv), audioEffect)

    case ServerMessage(s: SeqexecModelUpdate) =>
      // Replace the observer if not set and logged in
      val observer = value._2.map(_.displayName)
      val (sequencesWithObserver, effects) =
        s.view.queue.foldLeft(
          (List.empty[SequenceView],
           List(Some(Effect(Future(NoAction: Action)): Effect))
          )
        ) { case ((seq, eff), q) =>
            if (q.metadata.observer.isEmpty && observer.nonEmpty) {
              (q.copy(metadata = q.metadata.copy(observer = observer)) :: seq,
               Some(Effect(Future(UpdateObserver(q, observer.getOrElse("")): Action))) ::
               Some(Effect(Future(SyncToPage(q): Action))) :: eff)
            } else {
              (q :: seq, Some(Effect(Future(SyncToPage(q): Action))) :: eff)
            }
          }
      updated(value.copy(_1 = SequencesQueue(s.view.conditions, s.view.operator, sequencesWithObserver)),
              effects.flatten.reduce(_ + _): Effect)

    case ServerMessage(_) =>
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

  val seqexecQueueEq: FastEq[Pot[List[SequenceView]]] = potStateEq[List[SequenceView]]
  val searchResultsEq: FastEq[Pot[SearchResults]] = potStateEq[SearchResults]
  val sequenceEq: FastEq[Pot[SequenceView]] = potStateEq[SequenceView]
}

/**
  * Utility class to let components more easily switch parts of the UI depending on the context
  */
case class ClientStatus(u: Option[UserDetails], w: WebSocketConnection, anySelected: Boolean) extends UseValueEq {
  def isLogged: Boolean = u.isDefined
  def isConnected: Boolean = w.ws.isReady
}

case class HeaderSideBarReader(status: ClientStatus, conditions: Conditions, operator: Option[Operator]) extends UseValueEq
case class StatusAndLoadedSequences(isLogged: Boolean, sequences: LoadedSequences) extends UseValueEq
case class InstrumentSequence(tab: SequenceTab, active: Boolean) extends UseValueEq
case class InstrumentTabAndStatus(status: ClientStatus, tab: Option[InstrumentSequence]) extends UseValueEq

/**
  * Contains the model for Diode
  */
object SeqexecCircuit extends Circuit[SeqexecAppRootModel] with ReactConnector[SeqexecAppRootModel] {
  type SearchResults = SequencesQueue[SequenceId]
  private val logger = Logger.getLogger(SeqexecCircuit.getClass.getSimpleName)

  def appendToLogE(s: String) =
    Effect(Future(AppendToLog(s)))

  val wsHandler              = new WebSocketHandler(zoomTo(_.ws))
  val wsEventsHandler        = new WebSocketEventsHandler(zoomRW(m => (m.uiModel.sequences, m.uiModel.user))((m, v) => m.copy(uiModel = m.uiModel.copy(sequences = v._1, user = v._2))))
  val navigationHandler      = new NavigationHandler(zoomTo(_.uiModel.navLocation))
  val loginBoxHandler        = new LoginBoxHandler(zoomTo(_.uiModel.loginBox))
  val userLoginHandler       = new UserLoginHandler(zoomTo(_.uiModel.user))
  val sequenceDisplayHandler = new SequenceDisplayHandler(zoomTo(_.uiModel.sequencesOnDisplay))
  val sequenceExecHandler    = new SequenceExecutionHandler(zoomTo(_.uiModel.sequences))
  val globalLogHandler       = new GlobalLogHandler(zoomTo(_.uiModel.globalLog))
  val conditionsHandler      = new ConditionsHandler(zoomTo(_.uiModel.sequences.conditions))
  val operatorHandler        = new OperatorHandler(zoomTo(_.uiModel.sequences.operator))

  override protected def initialModel = SeqexecAppRootModel.initial

  // Some useful readers
  val statusAndLoadedSequences: ModelR[SeqexecAppRootModel, StatusAndLoadedSequences] =
    SeqexecCircuit.zoom(c => StatusAndLoadedSequences(c.uiModel.user.isDefined, c.uiModel.sequences))
  // Reader for sequences on display
  val sequencesOnDisplay: ModelR[SeqexecAppRootModel, SequencesOnDisplay] = zoom(_.uiModel.sequencesOnDisplay)
  val statusAndSequences: ModelR[SeqexecAppRootModel, (ClientStatus, SequencesOnDisplay)] = SeqexecCircuit.status.zip(SeqexecCircuit.sequencesOnDisplay)
  def headerSideBarReader: ModelR[SeqexecAppRootModel, HeaderSideBarReader] =
    SeqexecCircuit.zoom(c => HeaderSideBarReader(ClientStatus(c.uiModel.user, c.ws, c.uiModel.sequencesOnDisplay.isAnySelected), c.uiModel.sequences.conditions, c.uiModel.sequences.operator))

  def instrumentTab(i: Instrument): ModelR[SeqexecAppRootModel, Option[(SequenceTab, Boolean)]] = zoom(_.uiModel.sequencesOnDisplay.instrument(i))
  def instrumentTabAndStatus(i: Instrument): ModelR[SeqexecAppRootModel, InstrumentTabAndStatus] =
    status.zip(instrumentTab(i)).zoom {
      case (status, Some((tab, active))) => InstrumentTabAndStatus(status, InstrumentSequence(tab, active).some)
      case (status, _)                   => InstrumentTabAndStatus(status, none)
    }

  // Reader for a specific sequence if available
  def sequenceReader(id: SequenceId): ModelR[_, Option[SequenceView]] =
    zoom(_.uiModel.sequences.queue.find(_.id == id))

  // Reader to indicate the allowed interactions
  def status: ModelR[SeqexecAppRootModel, ClientStatus] = zoom(m => ClientStatus(m.uiModel.user, m.ws, m.uiModel.sequencesOnDisplay.isAnySelected))

  val statusAndConditions: ModelR[SeqexecAppRootModel, (ClientStatus, Conditions)] = SeqexecCircuit.status.zip(zoom(_.uiModel.sequences.conditions))

  /**
    * Makes a reference to a sequence on the queue.
    * This way we have a normalized model and need to update it in only one place
    */
  def sequenceRef(id: SequenceId): RefTo[Option[SequenceView]] =
    RefTo(sequenceReader(id))

  override protected def actionHandler = composeHandlers(
    wsHandler,
    wsEventsHandler,
    sequenceExecHandler,
    loginBoxHandler,
    userLoginHandler,
    sequenceDisplayHandler,
    globalLogHandler,
    conditionsHandler,
    operatorHandler,
    navigationHandler)

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
