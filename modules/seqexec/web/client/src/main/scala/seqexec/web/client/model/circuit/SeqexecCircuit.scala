// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.implicits._
import cats.data.NonEmptyList
import diode._
import diode.react.ReactConnector
import gem.Observation
import java.util.logging.Logger
import japgolly.scalajs.react.Callback
import seqexec.model._
import seqexec.model.events._
import seqexec.web.client.actions._
import seqexec.web.client.model._
import seqexec.web.client.lenses._
import seqexec.web.client.handlers._
import seqexec.web.client.ModelOps._
import seqexec.web.client.actions.{AppendToLog, CloseLoginBox, CloseUserNotificationBox, OpenLoginBox, OpenUserNotificationBox, ServerMessage, show}
import seqexec.web.client.components.sequence.steps.StepConfigTable
import web.client.table._

/**
 * Diode processor to log some of the action to aid in debugging
 */
final class LoggingProcessor[M <: AnyRef] extends ActionProcessor[M] {
  private val logger = Logger.getLogger(this.getClass.getName)
  override def process(dispatch: Dispatcher, action: Any, next: Any => ActionResult[M], currentModel: M): ActionResult[M] = {
    // log some of the actions
    action match {
      case AppendToLog(_)                     =>
      case ServerMessage(_: ServerLogMessage) =>
      case UpdateStepsConfigTableState(_)     =>
      case UpdateQueueTableState(_)           =>
      case UpdateStepTableState(_, _)         =>
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

  // Model read-writers
  val webSocketFocusRW: ModelRW[SeqexecAppRootModel, WebSocketsFocus] =
    zoomRW(m => WebSocketsFocus(m.uiModel.navLocation, m.uiModel.sequences, m.uiModel.user, m.uiModel.defaultObserver, m.clientId, m.site)) ((m, v) => m.copy(uiModel = m.uiModel.copy(sequences = v.sequences, user = v.user, defaultObserver = v.defaultObserver), clientId = v.clientId, site = v.site))

  val initialSyncFocusRW: ModelRW[SeqexecAppRootModel, InitialSyncFocus] =
    this.zoomRWL(SeqexecAppRootModel.uiModel ^|-> InitialSyncFocus.initialSyncFocusL)

  val tableStateRW: ModelRW[SeqexecAppRootModel, TableStates] =
    this.zoomRWL(SeqexecAppRootModel.uiModel ^|-> TableStates.tableStateL)

  // Reader to indicate the allowed interactions
  val statusReader: ModelR[SeqexecAppRootModel, ClientStatus] =
    this.zoomL(ClientStatus.clientStatusFocusL)

  // Reader to update the sequences in both parts of the model being used
  val sequencesReaderRW: ModelRW[SeqexecAppRootModel, SequencesFocus] =
    this.zoomRWL(SeqexecAppRootModel.uiModel ^|-> SequencesFocus.sequencesFocusL)

  // Reader to update the selected sequences and location
  val sodLocationReaderRW: ModelRW[SeqexecAppRootModel, SODLocationFocus] =
    this.zoomRWL(SODLocationFocus.sodLocationFocusL)

  // Some useful readers
  val statusAndLoadedSequencesReader: ModelR[SeqexecAppRootModel, StatusAndLoadedSequencesFocus] =
    statusReader.zip(zoom(_.uiModel.sequences.queue).zip(zoom(_.uiModel.sequencesOnDisplay).zip(zoom(_.uiModel.queueTableState)))).zoom {
      case (s, (queue, (sod, queueTable))) =>
        val sequencesInQueue = queue.map { s =>
          val active = sod.idDisplayed(s.id)
          val loaded = sod.loadedIds.contains(s.id)
          val targetName = firstScienceStepTargetNameT.headOption(s)
          SequenceInQueue(s.id, s.status, s.metadata.instrument, active, loaded, s.metadata.name, targetName, s.runningStep, s.nextStepToRun)
        }
        StatusAndLoadedSequencesFocus(s, sequencesInQueue.sorted, queueTable)
    }

  // Reader for sequences on display
  val headerSideBarReader: ModelR[SeqexecAppRootModel, HeaderSideBarFocus] =
    zoom { c =>
      val clientStatus = ClientStatus(c.uiModel.user, c.ws, c.uiModel.syncInProgress)
      val obs = c.uiModel.sequencesOnDisplay.selectedOperator match {
        case Some(x) => x.asRight
        case _       => c.uiModel.defaultObserver.asLeft
      }
      HeaderSideBarFocus(clientStatus, c.uiModel.sequences.conditions, c.uiModel.sequences.operator, obs)
    }


  val logDisplayedReader: ModelR[SeqexecAppRootModel, SectionVisibilityState] =
    this.zoomL(SeqexecAppRootModel.logDisplayL)

  val tabsReader: ModelR[SeqexecAppRootModel, TabFocus] = {
    val getter = SeqexecAppRootModel.uiModel composeGetter (SeqexecUIModel.sequencesOnDisplay composeGetter SequencesOnDisplay.availableTabsG).zip(SeqexecUIModel.defaultObserverG)
    val constructor = ClientStatus.canOperateG.zip(getter) >>> { p =>
      val (o, (t, ob)) = p
      TabFocus(o, t, ob)
    }

    this.zoomG(constructor)
  }

  val seqexecTabs: ModelR[SeqexecAppRootModel, NonEmptyList[TabContentFocus]] = {
    val getter = SeqexecAppRootModel.logDisplayL.asGetter.zip(SeqexecAppRootModel.sequencesOnDisplayL.asGetter)
    val constructor = ClientStatus.canOperateG.zip(getter) >>> { p =>
      val (o, (log, SequencesOnDisplay(tabs))) = p
      NonEmptyList.fromListUnsafe(tabs.withFocus.toList.collect {
        case (tab: SequenceTab, active)  => SequenceTabContentFocus(o, tab.instrument, tab.sequence.map(_.id), active, log)
        case (_: CalibrationQueueTab, _) => QueueTabContentFocus(o, log)
      })
    }

    this.zoomG(constructor)
  }

  val configTableState: ModelR[SeqexecAppRootModel, TableState[StepConfigTable.TableColumn]] =
    zoom(_.uiModel.configTableState)

  val sequencesOnDisplayRW: ModelRW[SeqexecAppRootModel, SequencesOnDisplay] =
    this.zoomRWL(SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.sequencesOnDisplay)

  def sequenceTab(id: Observation.Id): ModelR[SeqexecAppRootModel, SeqexecTabActive] =
    // Returning the getOrElse part shouldn't happen but it simplifies the model notcarrying the Option up
    zoom(_.uiModel.sequencesOnDisplay.tab(id).getOrElse(SeqexecTabActive.Empty))

  def sequenceObserverReader(id: Observation.Id): ModelR[SeqexecAppRootModel, SequenceInfoFocus] =
    statusReader.zip(sequenceTab(id)).zoom {
      case (status, SeqexecTabActive(tab, _)) =>
        val targetName = tab.sequence.flatMap(firstScienceStepTargetNameT.headOption)
        SequenceInfoFocus(status.isLogged, tab.sequence.map(_.metadata.name), tab.sequence.map(_.status), targetName)
    }

  def statusAndStepReader(id: Observation.Id): ModelR[SeqexecAppRootModel, Option[StatusAndStepFocus]] =
    statusReader.zip(sequenceTab(id)).zoom {
      case (status, SeqexecTabActive(tab, _)) =>
        tab.sequence.map { t =>
          StatusAndStepFocus(status.isLogged, t.metadata.instrument, t.id, tab.stepConfigDisplayed, t.steps.length, tab.isPreview)
        }
    }

  def stepsTableReaderF(id: Observation.Id): ModelR[SeqexecAppRootModel, Option[StepsTableFocus]] =
    sequenceTab(id).zoom {
      case SeqexecTabActive(tab, _) =>
        tab.sequence.map { sequence =>
          StepsTableFocus(sequence.id, sequence.metadata.instrument, sequence.status, sequence.steps, tab.stepConfigDisplayed, sequence.nextStepToRun, tab.isPreview, tab.tableState)
        }
    }

  def stepsTableReader(id: Observation.Id): ModelR[SeqexecAppRootModel, StepsTableAndStatusFocus] =
    statusReader.zip(stepsTableReaderF(id)).zip(configTableState).zoom {
      case ((s, f), t) => StepsTableAndStatusFocus(s, f, t)
    }(fastEq[StepsTableAndStatusFocus])

  def sequenceControlReader(obsId: Observation.Id): ModelR[SeqexecAppRootModel, SequenceControlFocus] =
    statusReader.zip(sequenceTab(obsId)).zoom {
      case (status, SeqexecTabActive(tab, _)) =>
        SequenceControlFocus(status.canOperate, ControlModel.controlModelG.get(tab), status.syncInProgress)
    }(fastEq[SequenceControlFocus])

  private val wsHandler                = new WebSocketHandler(zoomTo(_.ws))
  private val serverMessagesHandler    = new ServerMessagesHandler(webSocketFocusRW)
  private val initialSyncHandler       = new InitialSyncHandler(initialSyncFocusRW)
  private val navigationHandler        = new NavigationHandler(zoomTo(_.uiModel.navLocation))
  private val loginBoxHandler          = new ModalBoxHandler(OpenLoginBox, CloseLoginBox, zoomTo(_.uiModel.loginBox))
  private val notificationBoxHandler   = new ModalBoxHandler(OpenUserNotificationBox, CloseUserNotificationBox, zoomTo(_.uiModel.notification.visibility))
  private val userLoginHandler         = new UserLoginHandler(zoomTo(_.uiModel.user))
  private val userNotificationHandler  = new NotificationsHandler(zoomTo(_.uiModel.notification))
  private val sequenceDisplayHandler   = new SequenceDisplayHandler(sequencesReaderRW)
  private val sequenceExecHandler      = new SequenceExecutionHandler(zoomTo(_.uiModel.sequences))
  private val globalLogHandler         = new GlobalLogHandler(zoomTo(_.uiModel.globalLog))
  private val conditionsHandler        = new ConditionsHandler(zoomTo(_.uiModel.sequences.conditions))
  private val operatorHandler          = new OperatorHandler(zoomTo(_.uiModel.sequences.operator))
  private val defaultObserverHandler   = new DefaultObserverHandler(zoomTo(_.uiModel.defaultObserver))
  private val remoteRequestsHandler    = new RemoteRequestsHandler(zoomTo(_.clientId))
  private val syncRequestsHandler      = new SyncRequestsHandler(zoomTo(_.uiModel.syncInProgress))
  private val debuggingHandler         = new DebuggingHandler(zoomTo(_.uiModel.sequences))
  private val tableStateHandler        = new TableStateHandler(tableStateRW)
  private val loadSequencesHandler     = new LoadedSequencesHandler(sodLocationReaderRW)
  private val operationsStateHandler   = new OperationsStateHandler(sequencesOnDisplayRW)
  private val siteHandler              = new SiteHandler(zoomTo(_.site))

  def dispatchCB[A <: Action](a: A): Callback = Callback(dispatch(a))

  override protected def initialModel = SeqexecAppRootModel.Initial

  override protected def actionHandler = composeHandlers(
    wsHandler,
    foldHandlers(serverMessagesHandler, initialSyncHandler, loadSequencesHandler, userNotificationHandler),
    sequenceExecHandler,
    notificationBoxHandler,
    loginBoxHandler,
    userLoginHandler,
    sequenceDisplayHandler,
    globalLogHandler,
    conditionsHandler,
    operatorHandler,
    defaultObserverHandler,
    foldHandlers(remoteRequestsHandler, operationsStateHandler),
    syncRequestsHandler,
    navigationHandler,
    debuggingHandler,
    tableStateHandler,
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
