// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import diode._
import diode.data._
import diode.react.ReactConnector
import gem.Observation
import java.util.logging.Logger
import japgolly.scalajs.react.Callback
import seqexec.model._
import seqexec.model.events._
import seqexec.web.client.model._
import seqexec.web.client.lenses._
import seqexec.web.client.handlers._
import seqexec.web.client.ModelOps._
import seqexec.web.client.actions.{AppendToLog, CloseLoginBox, CloseResourcesBox, OpenLoginBox, OpenResourcesBox, ServerMessage, show}
import seqexec.web.client.components.sequence.steps.StepConfigTable
import web.client.table._

/**
 * Diode processor to log some of the action to aid in debugging
 */
final class LoggingProcessor[M <: AnyRef] extends ActionProcessor[M] {
  private val logger = Logger.getLogger(this.getClass.getName)
  override def process(dispatch: Dispatcher, action: Any, next: Any => ActionResult[M], currentModel: M): ActionResult[M] = {
    // log the action
    action match {
      case AppendToLog(_)                     =>
      case ServerMessage(_: ServerLogMessage) =>
      case a: Action                          => logger.info(s"Action: ${a.show}")
      case _                                  =>
    }
    // call the next processor
    next(action)
  }
}

/**
  * Contains the Diode circuit to manipulate the page
  */
object SeqexecCircuit extends Circuit[SeqexecAppRootModel] with ReactConnector[SeqexecAppRootModel] {
  private val logger = Logger.getLogger(SeqexecCircuit.getClass.getSimpleName)
  addProcessor(new LoggingProcessor[SeqexecAppRootModel]())

  implicit def fastEq[A: Eq]: FastEq[A] = new FastEq[A] {
    override def eqv(a: A, b: A): Boolean = a === b
  }

  implicit def fastNelEq[A: Eq]: FastEq[NonEmptyList[A]] = new FastEq[NonEmptyList[A]] {
    override def eqv(a: NonEmptyList[A], b: NonEmptyList[A]): Boolean = a === b
  }

  // Model read-writers
  val webSocketFocusRW: ModelRW[SeqexecAppRootModel, WebSocketsFocus] =
    zoomRW(m => WebSocketsFocus(m.uiModel.navLocation, m.uiModel.sequences, m.uiModel.user, m.clientId, m.site)) ((m, v) => m.copy(uiModel = m.uiModel.copy(sequences = v.sequences, user = v.user), clientId = v.clientId, site = v.site))

  val initialSyncFocusRW: ModelRW[SeqexecAppRootModel, InitialSyncFocus] =
    zoomRW(m => InitialSyncFocus(m.uiModel.navLocation, m.uiModel.firstLoad)) ((m, v) => m.copy(uiModel = m.uiModel.copy(navLocation = v.location, firstLoad = v.firstLoad)))

  val tableStateRW: ModelRW[SeqexecAppRootModel, TableStates] =
    zoomRW(m => TableStates(m.uiModel.queueTableState, m.uiModel.configTableState)) ((m, v) => m.copy(uiModel = m.uiModel.copy(queueTableState = v.queueTable, configTableState = v.stepConfigTable)))

  // Reader to indicate the allowed interactions
  val statusReader: ModelR[SeqexecAppRootModel, ClientStatus] = zoom(m => ClientStatus(m.uiModel.user, m.ws, m.uiModel.syncInProgress))

  // Some useful readers
  val statusAndLoadedSequencesReader: ModelR[SeqexecAppRootModel, StatusAndLoadedSequencesFocus] =
    statusReader.zip(zoom(_.uiModel.sequences.queue).zip(zoom(_.uiModel.sequencesOnDisplay).zip(zoom(_.uiModel.queueTableState)))).zoom {
      case (s, (queue, (sod, queueTable))) =>
        val sequencesInQueue = queue.map { s =>
          val active = sod.idDisplayed(s.id)
          val loaded = sod.loadedIds.contains(s.id)
          val targetName = firstScienceStepTargetNameT.headOption(s)
          SequenceInQueue(s.id, s.status, s.metadata.instrument, active, loaded, s.metadata.name, targetName, s.runningStep)
        }
        StatusAndLoadedSequencesFocus(s, sequencesInQueue.sorted, queueTable)
    }

  // Reader to contain the sequence in conflict
  val sequenceInConflictReader: ModelR[SeqexecAppRootModel, Option[Observation.Id]] = zoomTo(_.uiModel.resourceConflict.id)

  // Reader for sequences on display
  val headerSideBarReader: ModelR[SeqexecAppRootModel, HeaderSideBarFocus] =
    zoom(c => HeaderSideBarFocus(ClientStatus(c.uiModel.user, c.ws, c.uiModel.syncInProgress), c.uiModel.sequences.conditions, c.uiModel.sequences.operator))

  val logDisplayedReader: ModelR[SeqexecAppRootModel, SectionVisibilityState] =
    zoom(_.uiModel.globalLog.display)

  val tabsReader: ModelR[SeqexecAppRootModel, InstrumentTabFocus] =
    zoom(_.uiModel.user).zip(zoom(_.uiModel.sequencesOnDisplay.availableTabs)).zoom {
      case (u, tabs) => InstrumentTabFocus(tabs, u)
    }

  val sequenceTabs: ModelR[SeqexecAppRootModel, NonEmptyList[SequenceTabContentFocus]] =
    logDisplayedReader.zip(zoom(_.uiModel.sequencesOnDisplay)).zoom {
      case (log, SequencesOnDisplay(sequences)) => sequences.withFocus.map{
        case (tab, active) => SequenceTabContentFocus(tab.instrument, tab.sequence.map(_.id), active, log)
      }.toNel
    }

  val configTableState: ModelR[SeqexecAppRootModel, TableState[StepConfigTable.TableColumn]] =
    zoom(_.uiModel.configTableState)

  def sequenceTab(id: Observation.Id): ModelR[SeqexecAppRootModel, SequenceTabActive] =
    // Returning the getOrElse part shouldn't happen but it simplifies the model notcarrying the Option up
    zoom(_.uiModel.sequencesOnDisplay.tab(id).getOrElse(SequenceTabActive.Empty))

  def sequenceObserverReader(id: Observation.Id): ModelR[SeqexecAppRootModel, StatusAndObserverFocus] =
    statusReader.zip(sequenceTab(id)).zoom {
      case (status, SequenceTabActive(tab, _)) =>
        val targetName = tab.sequence.flatMap(firstScienceStepTargetNameT.headOption)
        StatusAndObserverFocus(status.isLogged, tab.sequence.map(_.metadata.name), id, tab.sequence.flatMap(_.metadata.observer), tab.sequence.map(_.status), targetName)
    }

  def statusAndStepReader(id: Observation.Id): ModelR[SeqexecAppRootModel, Option[StatusAndStepFocus]] =
    statusReader.zip(sequenceTab(id)).zoom {
      case (status, SequenceTabActive(tab, _)) =>
        tab.sequence.map { t =>
          StatusAndStepFocus(status.isLogged, t.metadata.instrument, t.id, tab.stepConfigDisplayed, t.steps.length, tab.isPreview)
        }
    }

  def stepsTableReaderF(id: Observation.Id): ModelR[SeqexecAppRootModel, Option[StepsTableFocus]] =
    sequenceTab(id).zoom {
      case SequenceTabActive(tab, _) =>
        tab.sequence.map { sequence =>
          StepsTableFocus(sequence.id, sequence.metadata.instrument, sequence.status, sequence.steps, tab.stepConfigDisplayed, sequence.nextStepToRun, tab.isPreview)
        }
    }

  def stepsTableReader(id: Observation.Id): ModelR[SeqexecAppRootModel, StepsTableAndStatusFocus] =
    statusReader.zip(stepsTableReaderF(id)).zip(configTableState).zoom {
      case ((s, f), t) => StepsTableAndStatusFocus(s, f, t)
    }

  def sequenceControlReader(obsId: Observation.Id): ModelR[SeqexecAppRootModel, SequenceControlFocus] =
    sequenceInConflictReader.zip(statusReader.zip(sequenceTab(obsId))).zoom {
      case (inConflict, (status, SequenceTabActive(tab, _))) =>
        SequenceControlFocus(status.isLogged, status.isConnected, tab.sequence.map(s => ControlModel(s.id, s.isPartiallyExecuted, s.nextStepToRun, s.status, inConflict.exists(_ === s.id))), status.syncInProgress)
    }

  // Reader for a specific sequence if available
  def sequenceReader(id: Observation.Id): ModelR[_, Option[SequenceView]] =
    zoom(_.uiModel.sequences.queue.find(_.id === id))

  /**
    * Makes a reference to a sequence on the queue.
    * This way we have a normalized model and need to update it in only one place
    */
  def sequenceRef(id: Observation.Id): RefTo[Option[SequenceView]] =
    RefTo(sequenceReader(id))

  private val wsHandler                = new WebSocketHandler(zoomTo(_.ws))
  private val serverMessagesHandler    = new ServerMessagesHandler(webSocketFocusRW)
  private val initialSyncHandler       = new InitialSyncHandler(initialSyncFocusRW)
  private val navigationHandler        = new NavigationHandler(zoomTo(_.uiModel.navLocation))
  private val loginBoxHandler          = new ModalBoxHandler(OpenLoginBox, CloseLoginBox, zoomTo(_.uiModel.loginBox))
  private val resourcesBoxHandler      = new ModalBoxHandler(OpenResourcesBox, CloseResourcesBox, zoomTo(_.uiModel.resourceConflict.visibility))
  private val userLoginHandler         = new UserLoginHandler(zoomTo(_.uiModel.user))
  private val sequenceDisplayHandler   = new SequenceDisplayHandler(zoomTo(_.uiModel.sequencesOnDisplay))
  private val sequenceExecHandler      = new SequenceExecutionHandler(zoomTo(_.uiModel.sequences))
  private val resourcesConflictHandler = new SequenceInConflictHandler(zoomTo(_.uiModel.resourceConflict.id))
  private val globalLogHandler         = new GlobalLogHandler(zoomTo(_.uiModel.globalLog))
  private val conditionsHandler        = new ConditionsHandler(zoomTo(_.uiModel.sequences.conditions))
  private val operatorHandler          = new OperatorHandler(zoomTo(_.uiModel.sequences.operator))
  // private val syncToAddedHandler       = new SyncToAddedRemovedRunHandler(zoomTo(_.uiModel.navLocation))
  private val remoteRequestsHandler    = new RemoteRequestsHandler(zoomTo(_.clientId))
  private val syncRequestsHandler      = new SyncRequestsHandler(zoomTo(_.uiModel.syncInProgress))
  private val debuggingHandler         = new DebuggingHandler(zoomTo(_.uiModel.sequences))
  private val stepConfigStateHandler   = new StepConfigTableStateHandler(tableStateRW)
  private val loadSequencesHandler     = new LoadedSequencesHandler(zoomTo(_.uiModel.sequencesOnDisplay))
  private val siteHandler              = new SiteHandler(zoomTo(_.site))

  def dispatchCB[A <: Action](a: A): Callback = Callback(dispatch(a))

  override protected def initialModel = SeqexecAppRootModel.Initial

  override protected def actionHandler = composeHandlers(
    wsHandler,
    foldHandlers(serverMessagesHandler, /*syncToAddedHandler,*/ initialSyncHandler, loadSequencesHandler),
    sequenceExecHandler,
    resourcesBoxHandler,
    resourcesConflictHandler,
    loginBoxHandler,
    userLoginHandler,
    sequenceDisplayHandler,
    globalLogHandler,
    conditionsHandler,
    operatorHandler,
    remoteRequestsHandler,
    syncRequestsHandler,
    navigationHandler,
    debuggingHandler,
    stepConfigStateHandler,
    siteHandler)

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
  }

}
