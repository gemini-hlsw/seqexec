// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client

import java.util.logging.{Level, Logger}
import java.time.Instant

import diode.util.RunAfterJS
import diode.{Action, ModelRW, NoAction, Effect, ActionHandler, ActionResult}
import diode.data.{Pending, Pot, Ready}

import edu.gemini.seqexec.model.{ModelBooPicklers, UserDetails}
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.ModelOps._
import edu.gemini.seqexec.web.client.actions._
import edu.gemini.seqexec.web.client.circuit._
import edu.gemini.seqexec.model.Model.SeqexecEvent.{ActionStopRequested, ConnectionOpenEvent, ObserverUpdated, SequenceCompleted}
import edu.gemini.seqexec.model.Model.SeqexecEvent.{ResourcesBusy, ServerLogMessage, SequenceLoaded, SequenceUnloaded}
import edu.gemini.seqexec.web.client.model.Pages._
import edu.gemini.seqexec.web.client.services.log.ConsoleHandler
import edu.gemini.seqexec.web.client.services.{SeqexecWebClient, Audio}
import edu.gemini.seqexec.web.client.model.SeqexecAppRootModel.LoadedSequences

import org.scalajs.dom._
import boopickle.Default._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scalaz._
import Scalaz._

object handlers {

  trait Handlers {
    implicit def pfMonoid[A, B]: Monoid[PartialFunction[A, B]] = new Monoid[PartialFunction[A, B]] {
      def zero = PartialFunction.empty[A, B]
      def append(pf1: PartialFunction[A, B], pf2: => PartialFunction[A, B]) = pf1.orElse(pf2)
    }
  }

  class NavigationHandler[M](modelRW: ModelRW[M, Pages.SeqexecPages]) extends ActionHandler(modelRW) with Handlers {
    def handleNavigateTo: PartialFunction[Any, ActionResult[M]] = {
      case NavigateTo(page) =>
        updated(page)
    }

    def handleSilentTo: PartialFunction[Any, ActionResult[M]] = {
      case NavigateSilentTo(page) =>
        val effect = page match {
          case InstrumentPage(i, None)     => Effect(Future(SelectInstrumentToDisplay(i)))
          case InstrumentPage(_, Some(id)) => Effect(Future(SelectIdToDisplay(id)))
          case _                           => Effect(Future(NoAction: Action))
        }
        updatedSilent(page, effect)
    }

    def handleSyncToPage: PartialFunction[Any, ActionResult[M]] = {
      case SyncToPage(s) =>
        // the page maybe not in sync with the tabs. Let's fix that
        value match {
          case InstrumentPage(i, Some(id)) if i === s.metadata.instrument && id === s.id =>
            effectOnly(Effect(Future(SelectIdToDisplay(s.id))))
          case _ =>
            noChange
        }
    }

    def handleSyncToRunning: PartialFunction[Any, ActionResult[M]] = {
      case SyncToRunning(s) =>
        // We'll select the sequence currently running and show the correct url
        value match {
          case Root | InstrumentPage(_, None) =>
              updated(InstrumentPage(s.metadata.instrument, s.id.some), Effect(Future(SelectInstrumentToDisplay(s.metadata.instrument))))
          case InstrumentPage(_, Some(id))    =>
            effectOnly(Effect(Future(SelectIdToDisplay(id))))
        }
    }

    def handleSyncPageToRemovedSequence: PartialFunction[Any, ActionResult[M]] = {
      case SyncPageToRemovedSequence(id) =>
        // If the id is selected, reset the route
        value match {
          case InstrumentPage(i, Some(sid)) if sid === id =>
            updated(InstrumentPage(i, none), Effect(Future(SelectInstrumentToDisplay(i))))
          case _                                          =>
            noChange
        }
    }

    def handleSyncPageToAddedSequence: PartialFunction[Any, ActionResult[M]] = {
      case SyncPageToAddedSequence(i, id) =>
        // Switch to the sequence in none is selected
        value match {
          case Root | InstrumentPage(_, None) =>
            updated(InstrumentPage(i, Some(id)), Effect(Future(SelectIdToDisplay(id))))
          case _                                  =>
            noChange
        }
    }

    def handle: PartialFunction[Any, ActionResult[M]] =
      List(handleNavigateTo,
        handleSilentTo,
        handleSyncToPage,
        handleSyncToRunning,
        handleSyncPageToRemovedSequence,
        handleSyncPageToAddedSequence).suml
  }

  /**
    * Handles sequence execution actions
    */
  class SequenceExecutionHandler[M](modelRW: ModelRW[M, LoadedSequences]) extends ActionHandler(modelRW) with Handlers {
    def handleRequestOperation: PartialFunction[Any, ActionResult[M]] = {
      case RequestRun(s) =>
        effectOnly(Effect(SeqexecWebClient.run(s).map(r => if (r.error) RunStartFailed(s) else RunStarted(s))))

      case RequestSync(s) =>
        effectOnly(Effect(SeqexecWebClient.sync(s).map(r => if (r.queue.isEmpty) RunSyncFailed(s) else RunSync(s))))

      case RequestPause(s) =>
        effectOnly(Effect(SeqexecWebClient.pause(s).map(r => if (r.error) RunPauseFailed(s) else RunPaused(s))))

      case RequestCancelPause(s) =>
        effectOnly(Effect(SeqexecWebClient.cancelPause(s).map(r => if (r.error) RunCancelPauseFailed(s) else RunCancelPaused(s))))

      case RequestStop(id, step) =>
        effectOnly(Effect(SeqexecWebClient.stop(id, step).map(r => if (r.error) RunStopFailed(id) else RunStop(id))))

      case RequestAbort(id, step) =>
        effectOnly(Effect(SeqexecWebClient.abort(id, step).map(r => if (r.error) RunAbortFailed(id) else RunAbort(id))))
    }

    def handleOperationResult: PartialFunction[Any, ActionResult[M]] = {
      case RunStarted(_) =>
        noChange

      case RunStartFailed(_) =>
        noChange

      case RunPaused(_) =>
        noChange

      case RunPauseFailed(_) =>
        noChange
    }

    def handleUpdateObserver: PartialFunction[Any, ActionResult[M]] = {
      case UpdateObserver(sequenceId, name) =>
        val updateObserverE = Effect(SeqexecWebClient.setObserver(sequenceId, name).map(_ => NoAction))
        val updatedSequences = value.copy(queue = value.queue.collect {
          case s if s.id === sequenceId =>
            s.copy(metadata = s.metadata.copy(observer = Some(Observer(name))))
          case s                  => s
        })
        updated(updatedSequences, updateObserverE)
    }

    def handleFlipSkipBreakpoint: PartialFunction[Any, ActionResult[M]] = {
      case FlipSkipStep(sequence, step) =>
        updated(value.copy(queue = value.queue.collect {
          case s if s.id === sequence => s.flipStep(step)
          case s                      => s
        }))

      case FlipBreakpointStep(sequenceId, step) =>
        val breakpointRequest = Effect(SeqexecWebClient.breakpoint(sequenceId, step.flipBreakpoint).map(_ => NoAction))
        updated(value.copy(queue = value.queue.collect {
          case s if s.id === sequenceId => s.flipBreakpointAtStep(step)
          case s                        => s
        }), breakpointRequest)
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(handleRequestOperation,
        handleOperationResult,
        handleUpdateObserver,
        handleFlipSkipBreakpoint).suml
  }

  /**
    * Handles actions related to opening/closing a modal
    */
  class ModalBoxHandler[M](openAction: Action, closeAction: Action, modelRW: ModelRW[M, SectionVisibilityState]) extends ActionHandler(modelRW) with Handlers {
    def openModal: PartialFunction[Any, ActionResult[M]] = {
      case x if x == openAction && value === SectionClosed =>
        updated(SectionOpen)

      case x if x == openAction                            =>
        noChange
    }

    def closeModal: PartialFunction[Any, ActionResult[M]] = {
      case x if x == closeAction && value === SectionOpen =>
        updated(SectionClosed)

      case x if x == closeAction                          =>
        noChange
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      openModal |+| closeModal
  }

  /**
    * Handles actions related to opening/closing the login box
    */
  class UserLoginHandler[M](modelRW: ModelRW[M, Option[UserDetails]]) extends ActionHandler(modelRW) with Handlers {
    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case LoggedIn(u) =>
        // Close the login box
        val effect = Effect(Future(CloseLoginBox))
        // Close the websocket and reconnect
        val reconnect = Effect(Future(Reconnect))
        updated(Some(u), reconnect + effect)

      case Logout =>
        val effect = Effect(SeqexecWebClient.logout().map(_ => NoAction))
        val reConnect = Effect(Future(Reconnect))
        // Remove the user and call logout
        updated(None, effect + reConnect)
    }
  }

  /**
    * Handles actions related to the changing the selection of the displayed sequence
    */
  class SequenceDisplayHandler[M](modelRW: ModelRW[M, (SequencesOnDisplay, LoadedSequences, SeqexecSite)]) extends ActionHandler(modelRW) with Handlers {
    def handleSelectSequenceDisplay: PartialFunction[Any, ActionResult[M]] = {
      case SelectInstrumentToDisplay(i) =>
        updated(value.copy(_1 = value._1.focusOnInstrument(i)))

      case SelectIdToDisplay(id) =>
        val seq = SeqexecCircuit.sequenceRef(id)
        updated(value.copy(_1 = value._1.focusOnSequence(seq)))
    }

    def handleInitialize: PartialFunction[Any, ActionResult[M]] = {
      case Initialize(site) =>
        updated(value.copy(_1 = value._1.withSite(site), _3 = site))
    }

    def handleShowHideStep: PartialFunction[Any, ActionResult[M]] = {
      case ShowStep(s, i) =>
        if (value._1.instrumentSequences.focus.sequence.exists(_.id === s)) {
          updated(value.copy(_1 = value._1.showStep(i)))
        } else {
          noChange
        }

      case UnShowStep(instrument) =>
        if (value._1.instrumentSequences.focus.sequence.exists(_.metadata.instrument == instrument)) {
          updated(value.copy(_1 = value._1.unshowStep))
        } else {
          noChange
        }
    }

    def handleRememberCompleted: PartialFunction[Any, ActionResult[M]] = {
      case RememberCompleted(s) =>
        updated(value.copy(_1 = value._1.markCompleted(s)))
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(handleSelectSequenceDisplay,
        handleInitialize,
        handleShowHideStep,
        handleRememberCompleted).suml
  }

  /**
   * Handles updates to the operator
   */
  class OperatorHandler[M](modelRW: ModelRW[M, Option[Operator]]) extends ActionHandler(modelRW) with Handlers {
    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case UpdateOperator(name) =>
        val updateOperatorE = Effect(SeqexecWebClient.setOperator(name).map(_ => NoAction))
        updated(name.some, updateOperatorE)
    }
  }

  /**
   * Handles updates to conditions
   */
  class ConditionsHandler[M](modelRW: ModelRW[M, Conditions]) extends ActionHandler(modelRW) with Handlers {
    val iqHandle: PartialFunction[Any, ActionResult[M]] = {
      case UpdateImageQuality(iq) =>
        val updateE = Effect(SeqexecWebClient.setImageQuality(iq).map(_ => NoAction))
        val updatedSequences = value.copy(iq = iq)
        updated(updatedSequences, updateE)
    }

    val ccHandle: PartialFunction[Any, ActionResult[M]] = {
      case UpdateCloudCover(cc) =>
        val updateE = Effect(SeqexecWebClient.setCloudCover(cc).map(_ => NoAction))
        val updatedSequences = value.copy(cc = cc)
        updated(updatedSequences, updateE)
    }

    val sbHandle: PartialFunction[Any, ActionResult[M]] = {
      case UpdateSkyBackground(sb) =>
        val updateE = Effect(SeqexecWebClient.setSkyBackground(sb).map(_ => NoAction))
        val updatedSequences = value.copy(sb = sb)
        updated(updatedSequences, updateE)
    }

    val wvHandle: PartialFunction[Any, ActionResult[M]] = {
      case UpdateWaterVapor(wv) =>
        val updateE = Effect(SeqexecWebClient.setWaterVapor(wv).map(_ => NoAction))
        val updatedSequences = value.copy(wv = wv)
        updated(updatedSequences, updateE)
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      iqHandle |+| ccHandle |+| sbHandle |+| wvHandle
  }

  /**
    * Handles updates to the log
    */
  class GlobalLogHandler[M](modelRW: ModelRW[M, GlobalLog]) extends ActionHandler(modelRW) with Handlers {
    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case AppendToLog(s) =>
        updated(value.copy(log = value.log.append(s)))
    }
  }

  /**
    * Handles setting what sequence is in conflict
    */
  class SequenceInConflictHandler[M](modelRW: ModelRW[M, Option[SequenceId]]) extends ActionHandler(modelRW) with Handlers {
    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case SequenceInConflict(id) =>
        updated(Some(id))
    }
  }

  /**
    * Handles the WebSocket connection and performs reconnection if needed
    */
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  class WebSocketHandler[M](modelRW: ModelRW[M, WebSocketConnection]) extends ActionHandler(modelRW) with Handlers with ModelBooPicklers {
    private implicit val runner = new RunAfterJS

    private val logger = Logger.getLogger(this.getClass.getSimpleName)
    // Reconfigure to avoid sending ajax events in this logger
    logger.setUseParentHandlers(false)
    logger.addHandler(new ConsoleHandler(Level.FINE))

    // Makes a websocket connection and setups event listeners
    def webSocket: Future[Action] = Future[Action] {
      import org.scalajs.dom.document

      val host = document.location.host
      val protocol = document.location.protocol.startsWith("https") ? "wss" | "ws"
      val url = s"$protocol://$host/api/seqexec/events"

      val ws = new WebSocket(url)

      def onOpen(): Unit = {
        logger.info(s"Connected to $url")
        SeqexecCircuit.dispatch(Connected(ws, 0))
      }

      def onMessage(e: MessageEvent): Unit = {
        e.data match {
          case buffer: ArrayBuffer =>
            val byteBuffer = TypedArrayBuffer.wrap(buffer)
            \/.fromTryCatchNonFatal(Unpickle[SeqexecEvent].fromBytes(byteBuffer)) match {
              case \/-(event) => logger.info(s"Decoding event: ${event.getClass}"); SeqexecCircuit.dispatch(ServerMessage(event))
              case -\/(t)     => logger.warning(s"Error decoding event ${t.getMessage}")
            }
          case _                   =>
            ()
        }
      }

      def onError(): Unit =
        // Unfortunately reading the event is not cross-browser safe
        logger.severe("Error on websocket")

      def onClose(): Unit =
        // Increase the delay to get exponential backoff with a minimum of 200ms and a max of 1m
        if (value.autoReconnect) {
          // On development mode reload when the connection is broken. This is quite ugly but it helps on development
          if (scala.scalajs.LinkingInfo.developmentMode) {
            // reload in 10 seconds
            scala.scalajs.js.timers.setTimeout(10000) (document.location.reload())
          }
          SeqexecCircuit.dispatch(ConnectionRetry(math.min(60000, math.max(200, value.nextAttempt * 2))))
        }

      ws.binaryType = "arraybuffer"
      ws.onopen = _ => onOpen
      ws.onmessage = onMessage _
      ws.onerror = _ => onError
      ws.onclose = _ => onClose
      Connecting
    }.recover {
      case _: Throwable => NoAction
    }

    def connectHandler: PartialFunction[Any, ActionResult[M]] = {
      case WSConnect(d) =>
        effectOnly(Effect(webSocket).after(d.millis))

      case Reconnect   =>
        // Capture the WS, or it maybe invalid during the Future
        val ws = value.ws
        val closeCurrent = Effect(Future(ws.foreach(_.close())).map(_ => NoAction))
        val reConnect = Effect(webSocket)
        updated(value.copy(ws = Pot.empty[WebSocket], nextAttempt = 0, autoReconnect = false), closeCurrent >> reConnect)
    }

    def connectingHandler: PartialFunction[Any, ActionResult[M]] = {
      case Connecting =>
        noChange
    }

    def connectedHandler: PartialFunction[Any, ActionResult[M]] = {
      case Connected(ws, delay) =>
        // After connected to the Websocket request a refresh
        val refreshRequest = Effect(SeqexecWebClient.refresh().map(_ => NoAction))
        updated(WebSocketConnection(Ready(ws), delay, autoReconnect = true), refreshRequest)
    }

    def connectionErrorHandler: PartialFunction[Any, ActionResult[M]] = {
      case ConnectionError(e) =>
        effectOnly(Effect.action(AppendToLog(ServerLogMessage(ServerLogLevel.ERROR, Instant.now, "Error connecting to the seqexec server"))))
    }

    def connectionClosedHandler: PartialFunction[Any, ActionResult[M]] = {
      case ConnectionRetry(next) =>
        logger.fine(s"Retry connecting in $next")
        val effect = Effect(Future(WSConnect(next)))
        updated(value.copy(ws = Pending(), nextAttempt = next), effect)
    }

    // This is essentially a state machine to handle the connection status and
    // can reconnect if needed
    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(connectHandler,
        connectingHandler,
        connectedHandler,
        connectionErrorHandler,
        connectionClosedHandler).suml
  }

  /**
    * Handles messages received over the WS channel
    */
  class WebSocketEventsHandler[M](modelRW: ModelRW[M, WebSocketsFocus]) extends ActionHandler(modelRW) with Handlers {
    private val VoidEffect = Effect(Future(NoAction))

    // It is legal do put sequences of the other sites on the queue
    // but we don't know how to display them, so let's filter them out
    private def filterSequences(sequences: LoadedSequences): LoadedSequences =
      sequences.copy(queue = sequences.queue.filter {
        case SequenceView(_, metadata, _, _, _) => value.site.instruments.list.toList.contains(metadata.instrument)
      })

    val logMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(l: ServerLogMessage) =>
        effectOnly(Effect(Future(AppendToLog(l))))
    }

    val connectionOpenMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(ConnectionOpenEvent(u)) =>
        updated(value.copy(user = u))
    }

    val sequenceCompletedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequenceCompleted(sv)) =>
        // Play audio when the sequence completes
        val audioEffect = Effect(Future(new Audio("/sequencecomplete.mp3").play()).map(_ => NoAction))
        val rememberCompleted = Effect(Future(sv.queue.find(_.status == SequenceState.Completed).fold(NoAction: Action)(RememberCompleted.apply)))
        updated(value.copy(sequences = filterSequences(sv)), audioEffect + rememberCompleted)
    }

    val observerUpdatedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(s: ObserverUpdated) =>
        updated(value.copy(sequences = filterSequences(s.view)))
    }

    val actionStoppedRequestMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(ActionStopRequested(svs)) =>
        updated(value.copy(sequences = filterSequences(svs)))
    }

    val resourceBusyMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(ResourcesBusy(id, sv)) =>
        val setConflictE = Effect(Future(SequenceInConflict(id)))
        val openBoxE = Effect(Future(OpenResourcesBox))
        effectOnly(setConflictE >> openBoxE)
    }

    val sequenceLoadedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequenceLoaded(id, view)) =>
        val observer = value.user.map(_.displayName)
        val newSequence = view.queue.find(_.id === id)
        val updateObserverE = (observer |@| newSequence) { (o, s) =>
            val updateObserver = Effect(Future(UpdateObserver(id, o)))
            if (view.queue.length == 1) {
              updateObserver + Effect(Future(SyncPageToAddedSequence(s.metadata.instrument, id)))
            } else {
              updateObserver
            }
          }.getOrElse(VoidEffect)
        updated(value.copy(sequences = filterSequences(view), firstLoad = false), updateObserverE)
    }
    val sequenceUnloadedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequenceUnloaded(id, view)) =>
        updated(value.copy(sequences = filterSequences(view), firstLoad = false), Effect(Future(SyncPageToRemovedSequence(id))))
    }

    val modelUpdateMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(s: SeqexecModelUpdate) =>
        // Replace the observer if not set and logged in
        val observer = value.user.map(_.displayName)
        val syncToRunE: Option[Effect] = (value.firstLoad option {
          s.view.queue.filter(_.status.isRunning) match {
             case x :: _   => Effect(Future(SyncToRunning(x))).some // if we have multiple sequences running, let's pick the first
             case _        => none
          }
        }).join
        val (sequencesWithObserver, effects) =
          filterSequences(s.view).queue.foldLeft(
            (List.empty[SequenceView],
             List[Option[Effect]](VoidEffect.some))
          ) { case ((seq, eff), q) =>
              val syncUrlE: Option[Effect] =
                syncToRunE.orElse(value.firstLoad option Effect(Future(SyncToPage(q)))).orElse(VoidEffect.some)
              if (q.metadata.observer.isEmpty && observer.nonEmpty) {
                (q.copy(metadata = q.metadata.copy(observer = observer.map(Observer.apply))) :: seq,
                Effect(Future(UpdateObserver(q.id, observer.getOrElse("")))).some ::
                syncUrlE :: eff)
              } else {
                (q :: seq, syncUrlE :: eff)
              }
            }
        val newValue = value.copy(sequences = SequencesQueue(s.view.conditions, s.view.operator, sequencesWithObserver), firstLoad = false)
        effects.collect { case Some(x) => x }.reduceOption(_ + _).fold(updated(newValue))(eff => updated(newValue, eff))
    }

    val defaultMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(_) =>
        // Ignore unknown events
        noChange
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(logMessage,
        connectionOpenMessage,
        sequenceCompletedMessage,
        observerUpdatedMessage,
        actionStoppedRequestMessage,
        sequenceLoadedMessage,
        sequenceUnloadedMessage,
        resourceBusyMessage,
        modelUpdateMessage,
        defaultMessage).suml
  }
}
