// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import boopickle.DefaultBasic._
import cats._
import cats.implicits._
import diode.util.RunAfterJS
import diode.{Action, ActionHandler, ActionResult, RootModelR, Effect, ModelRW, NoAction}
import diode.data.{Pending, RefTo, Pot, Ready}
import java.util.logging.{Level, Logger}
import java.time.Instant
import gem.Observation
import gem.enum.Site
import mouse.all._
import org.scalajs.dom._
import seqexec.model.UserDetails
import seqexec.model.boopickle._
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.events._
import seqexec.web.client.model._
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.SeqexecAppRootModel.LoadedSequences
import seqexec.web.client.lenses.{sequenceStepT, sequenceViewT}
import seqexec.web.client.ModelOps._
import seqexec.web.client.actions._
import seqexec.web.client.circuit._
import seqexec.web.client.services.log.ConsoleHandler
import seqexec.web.client.services.{Audio, SeqexecWebClient}
import seqexec.web.client.services.WebpackResources._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object handlers {
  private val VoidEffect = Effect(Future(NoAction: Action))

  trait Handlers {
    implicit def pfMonoid[A, B]: Monoid[PartialFunction[A, B]] = new Monoid[PartialFunction[A, B]] {
      override def empty = PartialFunction.empty[A, B]
      override def combine(x: PartialFunction[A, B], y: PartialFunction[A, B]): PartialFunction[A, B] = x.orElse(y)
    }
  }

  /**
   * Handles syncing added sequences to the page
   */
  class SyncToAddedRemovedRun[M](modelRW: ModelRW[M, Pages.SeqexecPages]) extends ActionHandler(modelRW) with Handlers {
    private def inInstrumentPage = value match {
      case Root | SoundTest | InstrumentPage(_) => true
      case _                                    => false
    }

    def handleSyncPageToAddedSequence: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequenceLoaded(id, view)) =>
        val newSequence = view.queue.find(_.id === id)
        // If a new sequence is loaded then switch the page and focus
        // if we are on the instrument route
        val toSelect = for {
          s <- newSequence
          if inInstrumentPage
        } yield s
        toSelect match {
          case Some(seq) =>
            val instrument = seq.metadata.instrument
            val sid = seq.id
            val step = seq.progress.last
            // We need to use an effect here as the model is not fully resolved
            val effect = Effect(Future(SelectIdToDisplay(sid)))
            value match {
              case Root | SoundTest | InstrumentPage(_) =>
                updated(SequencePage(instrument, id, step), effect)
              case _                                    =>
                effectOnly(effect)
            }
          case _ =>
            noChange
        }
      }

    def handleSyncPageToRemovedSequence: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequenceUnloaded(id, _)) =>
        // If the selecte id is removed, reset the route
        value match {
          case SequencePage(i, sid, _) if sid === id      =>
            updated(InstrumentPage(i))
          case _                                          =>
            noChange
        }
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(handleSyncPageToRemovedSequence, handleSyncPageToAddedSequence).combineAll

  }

  class NavigationHandler[M](modelRW: ModelRW[M, Pages.SeqexecPages]) extends ActionHandler(modelRW) with Handlers {
    def handleNavigateTo: PartialFunction[Any, ActionResult[M]] = {
      case NavigateTo(page) =>
        updated(page)
    }

    def handleSilentTo: PartialFunction[Any, ActionResult[M]] = {
      case NavigateSilentTo(page) =>
        val effect = page match {
          case InstrumentPage(i)               =>
            Effect(Future(SelectInstrumentToDisplay(i)))
          case SequencePage(_, id, _)          =>
            Effect(Future(SelectIdToDisplay(id)))
          case SequenceConfigPage(_, id, step) =>
            Effect(Future(ShowStepConfig(id, step)))
          case _                               =>
            VoidEffect
        }
        updatedSilent(page, effect)
    }

    def handle: PartialFunction[Any, ActionResult[M]] =
      List(handleNavigateTo, handleSilentTo).combineAll
  }

  /**
   * This handler is called only once. It will be triggered when the first message
   * with the full model arrives.
   * Then we sync to the first running sequence or to the route we are currently on
   */
  class InitialSyncHandler[M](modelRW: ModelRW[M, InitialSyncFocus]) extends ActionHandler(modelRW) with Handlers {
    def runningSequence(s: SeqexecModelUpdate): Option[SequenceView] =
      s.view.queue.filter(_.status.isRunning).sortBy(_.id).headOption

    def handle: PartialFunction[Any, ActionResult[M]] = {
      // If there is a running sequence update the page to go there
      case ServerMessage(s: SeqexecModelUpdate) if runningSequence(s).isDefined && value.firstLoad =>
        val running = runningSequence(s)
        running.fold(updated(value.copy(firstLoad = true))) { f =>
          // val seq = RefTo(new RootModelR(running))
          updated(value.copy(location = SequencePage(f.metadata.instrument, f.id, f.progress.last)))//, sod = value.sod.focusOnSequence(seq)))
        }

      // Otherwise, update the model to reflect the current page
      case ServerMessage(s: SeqexecModelUpdate) if value.firstLoad                                 =>
        // the page maybe not in sync with the tabs. Let's fix that
        val sids = s.view.queue.map(_.id)
        val instruments = s.view.queue.map(_.metadata.instrument)
        value.location match {
          case SequencePage(_, id, _) if sids.contains(id)                 =>
            // We are on a sequence page, update the model
            // val seq = RefTo(new RootModelR(s.view.queue.find(_.id === id)))
            // We need to effect to update the reference
            val effect = Effect(Future(SelectIdToDisplay(id)))
            updated(value.copy(/*sod = value.sod.focusOnSequence(seq), */firstLoad = false), effect)

          case InstrumentPage(instrument) if instruments.contains(instrument) =>
            // We are on a page for an instrument and we have sequences, let's go to the first one and change page
            val first = s.view.queue.filter(_.metadata.instrument == instrument).sortBy(_.id).headOption
            first.fold(updated(value.copy(firstLoad = false))) { f =>
              // val seq = RefTo(new RootModelR(first))
              val firstStep = f.progress.last
              // We need to effect to update the reference
              val effect = Effect(Future(SelectIdToDisplay(f.id)))
              // updated(value.copy(location = SequencePage(f.metadata.instrument, f.id, firstStep), sod = value.sod.focusOnSequence(seq), firstLoad = false), effect)
              updated(value.copy(location = SequencePage(f.metadata.instrument, f.id, firstStep), firstLoad = false), effect)
            }

          case SequenceConfigPage(_, id, step) if sids.contains(id)           =>
            // We are on a seq config page, update the model
            val seq = RefTo(new RootModelR(s.view.queue.find(_.id === id)))
            val effect = Effect(Future(ShowStepConfig(id, step)))
            updated(value.copy(sod = value.sod.focusOnSequence(seq).showStepConfig(step - 1), firstLoad = false), effect)
          case _                                                              =>
            // No matches
            updated(value.copy(firstLoad = false))
        }
      }
  }

  /**
  * Handles actions requesting sync
  */
  class SyncRequestsHandler[M](modelRW: ModelRW[M, Boolean]) extends ActionHandler(modelRW) with Handlers {
    def handleSyncRequestOperation: PartialFunction[Any, ActionResult[M]] = {
      case RequestSync(s) =>
        updated(true, Effect(SeqexecWebClient.sync(s).map(r => if (r.queue.isEmpty) RunSyncFailed(s) else RunSync(s))))
    }

    def handleSyncResult: PartialFunction[Any, ActionResult[M]] = {
      case RunSyncFailed(_) =>
        updated(false)

      case RunSync(_) =>
        updated(false)
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(handleSyncRequestOperation,
        handleSyncResult).combineAll
  }

  /**
  * Handles actions requesting results
  */
  class RemoteRequestsHandler[M](modelRW: ModelRW[M, Option[ClientID]]) extends ActionHandler(modelRW) with Handlers {
    def handleRequestOperation: PartialFunction[Any, ActionResult[M]] = {
      case RequestRun(s) =>
        val effect = value.map(clientId => Effect(SeqexecWebClient.run(s, clientId).map(r => if (r.error) RunStartFailed(s) else RunStarted(s)))).getOrElse(VoidEffect)
        effectOnly(effect)

      case RequestPause(s) =>
        effectOnly(Effect(SeqexecWebClient.pause(s).map(r => if (r.error) RunPauseFailed(s) else RunPaused(s))))

      case RequestCancelPause(s) =>
        effectOnly(Effect(SeqexecWebClient.cancelPause(s).map(r => if (r.error) RunCancelPauseFailed(s) else RunCancelPaused(s))))

      case RequestStop(id, step) =>
        effectOnly(Effect(SeqexecWebClient.stop(id, step).map(r => if (r.error) RunStopFailed(id) else RunStop(id))))

      case RequestAbort(id, step) =>
        effectOnly(Effect(SeqexecWebClient.abort(id, step).map(r => if (r.error) RunAbortFailed(id) else RunAbort(id))))

      case RequestObsPause(id, step) =>
        effectOnly(Effect(SeqexecWebClient.pauseObs(id, step).map(r => if (r.error) RunObsPauseFailed(id) else RunObsPause(id))))

      case RequestObsResume(id, step) =>
        effectOnly(Effect(SeqexecWebClient.resumeObs(id, step).map(r => if (r.error) RunObsResumeFailed(id) else RunObsPause(id))))

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

    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(handleRequestOperation,
        handleOperationResult).combineAll
  }

  /**
    * Handles sequence execution actions
    */
  class SequenceExecutionHandler[M](modelRW: ModelRW[M, LoadedSequences]) extends ActionHandler(modelRW) with Handlers {
    def handleUpdateObserver: PartialFunction[Any, ActionResult[M]] = {
      case UpdateObserver(sequenceId, name) =>
        val updateObserverE = Effect(SeqexecWebClient.setObserver(sequenceId, name).map(_ => NoAction))
        val updatedSequences = value.copy(queue = value.queue.collect {
          case s if s.id === sequenceId =>
            s.copy(metadata = s.metadata.copy(observer = Some(Observer(name))))
          case s                        => s
        })
        updated(updatedSequences, updateObserverE)
    }

    def handleFlipSkipBreakpoint: PartialFunction[Any, ActionResult[M]] = {
      case FlipSkipStep(sequenceId, step) =>
        val skipRequest = Effect(SeqexecWebClient.skip(sequenceId, step.flipSkip).map(_ => NoAction))
        updated(value.copy(queue = value.queue.collect {
          case s if s.id === sequenceId => s.flipSkipMarkAtStep(step)
          case s                        => s
        }), skipRequest)

      case FlipBreakpointStep(sequenceId, step) =>
        val breakpointRequest = Effect(SeqexecWebClient.breakpoint(sequenceId, step.flipBreakpoint).map(_ => NoAction))
        updated(value.copy(queue = value.queue.collect {
          case s if s.id === sequenceId => s.flipBreakpointAtStep(step)
          case s                        => s
        }), breakpointRequest)
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(handleUpdateObserver, handleFlipSkipBreakpoint).combineAll
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
  class SequenceDisplayHandler[M](modelRW: ModelRW[M, (SequencesOnDisplay, Option[Site])]) extends ActionHandler(modelRW) with Handlers {
    def handleSelectSequenceDisplay: PartialFunction[Any, ActionResult[M]] = {
      case SelectInstrumentToDisplay(_) =>
        noChange
        // updated(value.copy(_1 = value._1.focusOnInstrument(i)))

      case SelectIdToDisplay(_) =>
        // val seq = SeqexecCircuit.sequenceRef(id)
        // updated(value.copy(_1 = value._1.focusOnSequence(seq)))
        noChange

    }

    def handleInitialize: PartialFunction[Any, ActionResult[M]] = {
      case Initialize(site) =>
        updated(value.copy(_1 = value._1.withSite(site), _2 = Some(site)))
    }

    // def handleShowHideStep: PartialFunction[Any, ActionResult[M]] = {
    //   case ShowStepConfig(id, step)         =>
    //     val seq = SeqexecCircuit.sequenceRef(id)
    //     updated(value.copy(_1 = value._1.focusOnSequence(seq).showStepConfig(step - 1)))
    //
    //   case HideStepConfig(instrument) =>
    //     if (value._1.instrumentSequences.focus.sequence.exists(_.metadata.instrument == instrument)) {
    //       updated(value.copy(_1 = value._1.hideStepConfig))
    //     } else {
    //       noChange
    //     }
    // }

    def handleRememberCompleted: PartialFunction[Any, ActionResult[M]] = {
      case RememberCompleted(s) =>
        updated(value.copy(_1 = value._1.markCompleted(s)))
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(handleSelectSequenceDisplay,
        handleInitialize,
        // handleShowHideStep,
        handleRememberCompleted).combineAll
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
   * Handles updates to the selected sequences set
   */
  class ClientsModelHandler[M](modelRW: ModelRW[M, SequencesOnDisplay]) extends ActionHandler(modelRW) with Handlers {
    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SelectedSequenceUpdated(svs)) =>
        println(svs.selected)
        noChange

      case LoadSequence(i, id) =>
        effectOnly(Effect(SeqexecWebClient.loadSequence(i, id).map(r => if (r.error) NoAction else NoAction)))
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

      case ToggleLogArea =>
        updated(value.copy(display = value.display.toggle))
    }
  }

  /**
    * Handles setting what sequence is in conflict
    */
  class SequenceInConflictHandler[M](modelRW: ModelRW[M, Option[Observation.Id]]) extends ActionHandler(modelRW) with Handlers {
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
      val host = document.location.host
      val protocol = document.location.protocol.startsWith("https").fold("wss", "ws")
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
            Either.catchNonFatal(Unpickle[SeqexecEvent].fromBytes(byteBuffer)) match {
              case Right(event: ServerLogMessage) =>
                SeqexecCircuit.dispatch(ServerMessage(event))
              case Right(event)                   =>
                logger.info(s"Decoding event: ${event.getClass}")
                SeqexecCircuit.dispatch(ServerMessage(event))
              case Left(t)                       =>
                logger.warning(s"Error decoding event ${t.getMessage}")
            }
          case _                   =>
            ()
        }
      }

      def onError(): Unit = logger.severe("Error on websocket")

      def onClose(): Unit =
        // Increase the delay to get exponential backoff with a minimum of 200ms and a max of 1m
        if (value.autoReconnect) {
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
        updated(WebSocketConnection(Ready(ws), delay, autoReconnect = true))
    }

    def connectedCloseHandler: PartialFunction[Any, ActionResult[M]] = {
      case WSClose =>
        // Forcefully close the websocket as requested when reloading the code via HMR
        val ws = value.ws
        val closeEffect = Effect(Future(ws.foreach(_.close())).map(_ => NoAction))
        updated(value.copy(ws = Pot.empty[WebSocket], nextAttempt = 0, autoReconnect = false), closeEffect)
    }

    def connectionErrorHandler: PartialFunction[Any, ActionResult[M]] = {
      case ConnectionError(_) =>
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
        connectedCloseHandler,
        connectionClosedHandler).combineAll
  }

  /**
    * Handles messages received over the WS channel
    */
  class WebSocketEventsHandler[M](modelRW: ModelRW[M, WebSocketsFocus]) extends ActionHandler(modelRW) with Handlers {
    // Global references to audio files
    private val SequencePausedAudio = new Audio(SequencePausedResource.resource)
    private val ExposurePausedAudio = new Audio(ExposurePausedResource.resource)
    private val SequenceErrorAudio = new Audio(SequenceErrorResource.resource)
    private val SequenceCompleteAudio = new Audio(SequenceCompleteResource.resource)
    private val StepBeepAudio = new Audio(BeepResource.resource)

    // It is legal do put sequences of the other sites on the queue
    // but we don't know how to display them, so let's filter them out
    private def filterSequences(sequences: LoadedSequences): LoadedSequences =
      sequences.copy(queue = sequences.queue.filter {
        case SequenceView(_, metadata, _, _, _) => value.site.map(_.instruments.toList.contains(metadata.instrument)).getOrElse(false)
      })

    val soundCheck: PartialFunction[Any, ActionResult[M]] = {
      case RequestSoundEcho =>
        val soundEffect = Effect(Future(SequenceCompleteAudio.play()).map(_ => NoAction))
        effectOnly(soundEffect)
    }

    val logMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(l: ServerLogMessage) =>
        effectOnly(Effect(Future(AppendToLog(l))))
    }

    val connectionOpenMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(ConnectionOpenEvent(u, c)) =>
        // After connected to the Websocket request a refresh
        val refreshRequest = Effect(SeqexecWebClient.refresh(c).map(_ => NoAction))
        updated(value.copy(user = u, clientId = Option(c)), refreshRequest)
    }

    val stepCompletedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(e @ StepExecuted(obsId, sv)) =>
      val curStep =
        for {
            obs      <- sequenceViewT.find(_.id === obsId)(e)
            curSIdx  <- obs.runningStep.map(_.last)
            curStep  <- sequenceStepT.find(_.id === curSIdx)(obs)
            if curStep.observeStatus === ActionStatus.Pending && curStep.status === StepState.Running
            if curStep.configStatus.map(_._2).forall(_ === ActionStatus.Pending)
          } yield curStep

        val audioEffect = curStep.fold(VoidEffect)(_ => Effect(Future(StepBeepAudio.play()).map(_ => NoAction)))
        updated(value.copy(sequences = filterSequences(sv)), audioEffect)
    }

    val sequenceCompletedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequenceCompleted(sv)) =>
        // Play audio when the sequence completes
        val audioEffect = Effect(Future(SequenceCompleteAudio.play()).map(_ => NoAction))
        val rememberCompleted = Effect(Future(sv.queue.find(_.status == SequenceState.Completed).fold(NoAction: Action)(RememberCompleted.apply)))
        updated(value.copy(sequences = filterSequences(sv)), audioEffect + rememberCompleted)
    }

    val sequenceOnErrorMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequenceError(_, sv)) =>
        // Play audio when the sequence gets into an error state
        val audioEffect = Effect(Future(SequenceErrorAudio.play()).map(_ => NoAction))
        updated(value.copy(sequences = filterSequences(sv)), audioEffect)
    }

    val sequencePausedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequencePaused(_, sv)) =>
        // Play audio when the sequence gets paused
        val audioEffect = Effect(Future(SequencePausedAudio.play()).map(_ => NoAction))
        updated(value.copy(sequences = filterSequences(sv)), audioEffect)
    }

    val exposurePausedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(ExposurePaused(_, sv)) =>
        // Play audio when the sequence gets paused
        val audioEffect = Effect(Future(ExposurePausedAudio.play()).map(_ => NoAction))
        updated(value.copy(sequences = filterSequences(sv)), audioEffect)
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
      case ServerMessage(ResourcesBusy(id, _, _)) =>
        val setConflictE = Effect(Future(SequenceInConflict(id)))
        val openBoxE = Effect(Future(OpenResourcesBox))
        effectOnly(setConflictE >> openBoxE)
    }

    val sequenceLoadedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequenceLoaded(id, view)) =>
        val observer = value.user.map(_.displayName)
        val updateObserverE = observer.fold(VoidEffect)(o => Effect(Future(UpdateObserver(id, o): Action)))
        updated(value.copy(sequences = filterSequences(view)), updateObserverE)
    }

    val sequenceUnloadedMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(SequenceUnloaded(_, view)) =>
        updated(value.copy(sequences = filterSequences(view)))
    }

    val modelUpdateMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(s: SeqexecModelUpdate) =>
        // Replace the observer if not set and logged in
        val observer = value.user.map(_.displayName)
        val newQueue = filterSequences(s.view).queue
        val sequencesWithObserver = newQueue.collect {
          case q if q.metadata.observer.isEmpty && observer.nonEmpty =>
            q.copy(metadata = q.metadata.copy(observer = observer.map(Observer.apply)))
          case q                                                     =>
            q
        }
        val effects = newQueue.collect {
          case q if q.metadata.observer.isEmpty && observer.nonEmpty =>
            Effect(Future(UpdateObserver(q.id, observer.getOrElse("")))).some
        }
        val newValue = value.copy(sequences = SequencesQueue(s.view.selected, s.view.conditions, s.view.operator, sequencesWithObserver))
        effects.reduceOption(_ >> _).fold(updated(newValue))(eff => updated(newValue, eff.getOrElse(VoidEffect)))
    }

    val defaultMessage: PartialFunction[Any, ActionResult[M]] = {
      case ServerMessage(_) =>
        // Ignore unknown events
        noChange
    }

    override def handle: PartialFunction[Any, ActionResult[M]] =
      List(soundCheck,
        logMessage,
        stepCompletedMessage,
        connectionOpenMessage,
        sequenceCompletedMessage,
        sequenceOnErrorMessage,
        sequencePausedMessage,
        exposurePausedMessage,
        observerUpdatedMessage,
        actionStoppedRequestMessage,
        sequenceLoadedMessage,
        sequenceUnloadedMessage,
        resourceBusyMessage,
        modelUpdateMessage,
        defaultMessage).combineAll
  }

  /**
    * Handle for UI debugging events
    */
  class DebuggingHandler[M](modelRW: ModelRW[M, LoadedSequences]) extends ActionHandler(modelRW) with Handlers {
    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case MarkStepAsRunning(obsId, step) =>
        updated(value.copy(queue = value.queue.collect {
          case v: SequenceView if v.id === obsId => v.showAsRunning(step)
          case v                                 => v
        }))
    }
  }

  /**
    * Handle to preserve the steps table state
    */
  class StepConfigTableStateHandler[M](modelRW: ModelRW[M, TableStates]) extends ActionHandler(modelRW) with Handlers {
    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case UpdateStepsConfigTableState(state) =>
        updatedSilent(value.copy(stepConfigTable = state)) // We should only do silent updates as these change too quickly

      case UpdateQueueTableState(state) =>
        updatedSilent(value.copy(queueTable = state)) // We should only do silent updates as these change too quickly
    }
  }
}
