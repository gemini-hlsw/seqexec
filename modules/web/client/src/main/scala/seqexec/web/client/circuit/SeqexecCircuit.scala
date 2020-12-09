// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import scala.scalajs.LinkingInfo

import cats._
import cats.data.NonEmptyList
import cats.syntax.all._
import diode._
import diode.react.ReactConnector
import japgolly.scalajs.react.Callback
import monocle.Prism
import seqexec.model.Observation
import seqexec.model._
import seqexec.model.events._
import seqexec.web.client.actions.AppendToLog
import seqexec.web.client.actions.CloseLoginBox
import seqexec.web.client.actions.CloseUserNotificationBox
import seqexec.web.client.actions.OpenLoginBox
import seqexec.web.client.actions.OpenUserNotificationBox
import seqexec.web.client.actions.ServerMessage
import seqexec.web.client.actions._
import seqexec.web.client.actions.show
import seqexec.web.client.handlers._
import seqexec.web.client.model._
import typings.loglevel.mod.{ ^ => logger }

/**
 * Diode processor to log some of the action to aid in debugging
 */
final class LoggingProcessor[M <: AnyRef] extends ActionProcessor[M] {
  override def process(
    dispatch:     Dispatcher,
    action:       Any,
    next:         Any => ActionResult[M],
    currentModel: M
  ): ActionResult[M] = {
    // log some of the actions
    action match {
      case AppendToLog(_)                             =>
      case ServerMessage(_: ServerLogMessage)         =>
      case ServerMessage(_: ObservationProgressEvent) =>
      case UpdateStepsConfigTableState(_)             =>
      case UpdateSessionQueueTableState(_)            =>
      case UpdateStepTableState(_, _)                 =>
      case UpdateCalTableState(_, _)                  =>
      case UpdateSelectedStep(_, _)                   =>
      case VerifyLoggedStatus                         =>
      case a: Action                                  =>
        if (LinkingInfo.developmentMode) logger.info(s"Action: ${a.show}")
      case _                                          =>
    }
    // call the next processor
    next(action)
  }
}

/**
 * Contains the Diode circuit to manipulate the page
 */
object SeqexecCircuit
    extends Circuit[SeqexecAppRootModel]
    with ReactConnector[SeqexecAppRootModel] {
  addProcessor(new LoggingProcessor[SeqexecAppRootModel]())

  // Model read-writers
  val webSocketFocusRW: ModelRW[SeqexecAppRootModel, WebSocketsFocus] =
    this.zoomRWL(WebSocketsFocus.webSocketFocusL)

  val initialSyncFocusRW: ModelRW[SeqexecAppRootModel, InitialSyncFocus] =
    this.zoomRWL(SeqexecAppRootModel.uiModel ^|-> InitialSyncFocus.initialSyncFocusL)

  val tableStateRW: ModelRW[SeqexecAppRootModel, AppTableStates] =
    this.zoomRWL(SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.appTableStates)

  // Reader to indicate the allowed interactions
  val statusReader: ModelR[SeqexecAppRootModel, ClientStatus] =
    this.zoomL(ClientStatus.clientStatusFocusL)

  // Reader to read/write the sound setting
  val soundSettingReader: ModelR[SeqexecAppRootModel, SoundSelection] =
    this.zoomL(SeqexecAppRootModel.soundSettingL)

  // Reader for the queue operations
  val queueOperationsRW: ModelRW[SeqexecAppRootModel, CalibrationQueues] =
    this.zoomRWL(SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.queues)

  // Reader to update the sequences in both parts of the model being used
  val sequencesReaderRW: ModelRW[SeqexecAppRootModel, SequencesFocus] =
    this.zoomRWL(SequencesFocus.sequencesFocusL)

  // Reader to update the selected sequences and location
  val sodLocationReaderRW: ModelRW[SeqexecAppRootModel, SODLocationFocus] =
    this.zoomRWL(SODLocationFocus.sodLocationFocusL)

  val statusAndLoadedSequencesReader: ModelR[SeqexecAppRootModel, StatusAndLoadedSequencesFocus] =
    this.zoomG(StatusAndLoadedSequencesFocus.statusAndLoadedSequencesG)

  val sessionQueueFilterReader: ModelR[SeqexecAppRootModel, SessionQueueFilter] =
    this.zoomL(SeqexecAppRootModel.sessionQueueFilterL)

  // Reader for sequences on display
  val headerSideBarReader: ModelR[SeqexecAppRootModel, HeaderSideBarFocus] =
    this.zoomG(HeaderSideBarFocus.headerSideBarG)

  val logDisplayedReader: ModelR[SeqexecAppRootModel, SectionVisibilityState] =
    this.zoomL(SeqexecAppRootModel.logDisplayL)

  val tabsReader: ModelR[SeqexecAppRootModel, TabFocus] =
    this.zoomG(TabFocus.tabFocusG)

  val seqexecTabs: ModelR[SeqexecAppRootModel, NonEmptyList[TabContentFocus]] =
    this.zoomG(TabContentFocus.tabContentFocusG)

  val sequencesOnDisplayRW: ModelRW[SeqexecAppRootModel, SequencesOnDisplay] =
    this.zoomRWL(SeqexecAppRootModel.sequencesOnDisplayL)

  val queueFocusRW: ModelRW[SeqexecAppRootModel, QueueRequestsFocus] =
    this.zoomRWL(QueueRequestsFocus.unsafeQueueRequestsFocusL)

  val acProgressRW: ModelRW[SeqexecAppRootModel, AlignAndCalibStep] =
    this.zoomRWL(SeqexecAppRootModel.alignAndCalib)

  def sequenceTab(
    id: Observation.Id
  ): ModelR[SeqexecAppRootModel, Option[SeqexecTabActive]] =
    this.zoomG(
      SeqexecAppRootModel.sequencesOnDisplayL
        .composeGetter(SequencesOnDisplay.tabG(id))
    )

  def sequenceObserverReader(
    id: Observation.Id
  ): ModelR[SeqexecAppRootModel, Option[SequenceInfoFocus]] =
    this.zoomG(SequenceInfoFocus.sequenceInfoG(id))

  def obsProgressReader[P <: Progress: Eq](
    id:                     Observation.Id,
    stepId:                 StepId
  )(implicit progressPrism: Prism[Progress, P]): ModelR[SeqexecAppRootModel, Option[P]] =
    this.zoomO(AllObservationsProgressState.progressStateO[P](id, stepId))

  def statusAndStepReader(
    id: Observation.Id
  ): ModelR[SeqexecAppRootModel, Option[StatusAndStepFocus]] =
    this.zoomG(StatusAndStepFocus.statusAndStepG(id))

  def stepsTableReaderF(
    id: Observation.Id
  ): ModelR[SeqexecAppRootModel, Option[StepsTableFocus]] =
    this.zoomG(StepsTableFocus.stepsTableG(id))

  def stepsTableReader(
    id: Observation.Id
  ): ModelR[SeqexecAppRootModel, StepsTableAndStatusFocus] =
    this.zoomG(StepsTableAndStatusFocus.stepsTableAndStatusFocusG(id))

  def sequenceControlReader(
    id: Observation.Id
  ): ModelR[SeqexecAppRootModel, Option[SequenceControlFocus]] =
    this.zoomG(SequenceControlFocus.seqControlG(id))

  def calQueueControlReader(
    id: QueueId
  ): ModelR[SeqexecAppRootModel, Option[CalQueueControlFocus]] =
    this.zoomG(CalQueueControlFocus.queueControlG(id))

  def calQueueReader(
    id: QueueId
  ): ModelR[SeqexecAppRootModel, Option[CalQueueFocus]] =
    this.zoomG(CalQueueFocus.calQueueG(id))

  private val wsHandler               = new WebSocketHandler(zoomTo(_.ws))
  private val serverMessagesHandler   = new ServerMessagesHandler(webSocketFocusRW)
  private val initialSyncHandler      = new InitialSyncHandler(initialSyncFocusRW)
  private val navigationHandler       = new NavigationHandler(zoomTo(_.uiModel.navLocation))
  private val loginBoxHandler         =
    new ModalBoxHandler(OpenLoginBox, CloseLoginBox, zoomTo(_.uiModel.loginBox))
  private val notificationBoxHandler  = new ModalBoxHandler(OpenUserNotificationBox,
                                                           CloseUserNotificationBox,
                                                           zoomTo(_.uiModel.notification.visibility)
  )
  private val userLoginHandler        = new UserLoginHandler(zoomTo(_.uiModel.user))
  private val userNotificationHandler = new NotificationsHandler(zoomTo(_.uiModel.notification))
  private val userPromptHandler       = new UserPromptHandler(zoomTo(_.uiModel.userPrompt))
  private val sequenceDisplayHandler  = new SequenceDisplayHandler(sequencesReaderRW)
  private val sequenceExecHandler     = new SequenceExecutionHandler(zoomTo(_.sequences))
  private val globalLogHandler        = new GlobalLogHandler(zoomTo(_.uiModel.globalLog))
  private val conditionsHandler       = new ConditionsHandler(zoomTo(_.sequences.conditions))
  private val operatorHandler         = new OperatorHandler(zoomTo(_.sequences.operator))
  private val defaultObserverHandler  = new DefaultObserverHandler(zoomTo(_.uiModel.defaultObserver))
  private val remoteRequestsHandler   = new RemoteRequestsHandler(zoomTo(_.clientId))
  private val queueRequestsHandler    = new QueueRequestsHandler(queueFocusRW)
  private val tableStateHandler       = new TableStateHandler(tableStateRW)
  private val loadSequencesHandler    = new LoadedSequencesHandler(sodLocationReaderRW)
  private val operationsStateHandler  = new OperationsStateHandler(sequencesOnDisplayRW)
  private val siteHandler             = new SiteHandler(zoomTo(_.site))
  private val queueOpsHandler         = new QueueOperationsHandler(queueOperationsRW)
  private val queueStateHandler       = new QueueStateHandler(queueOperationsRW)
  private val openConnectionHandler   = new OpenConnectionHandler(zoomTo(_.uiModel.queues))
  private val observationsProgHandler = new ObservationsProgressStateHandler(
    zoomTo(_.uiModel.obsProgress)
  )
  private val sessionFilterHandler    = new SessionQueueFilterHandler(
    zoomTo(_.uiModel.sessionQueueFilter)
  )
  private val soundHandler            = new SoundOnOffHandler(zoomTo(_.uiModel.sound))

  def dispatchCB[A <: Action](a: A): Callback = Callback(dispatch(a))

  override protected def initialModel = SeqexecAppRootModel.Initial

  override protected def actionHandler =
    composeHandlers(
      wsHandler,
      foldHandlers(
        serverMessagesHandler,
        initialSyncHandler,
        loadSequencesHandler,
        userNotificationHandler,
        userPromptHandler,
        openConnectionHandler,
        queueStateHandler,
        observationsProgHandler
      ),
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
      foldHandlers(queueOpsHandler, queueRequestsHandler),
      navigationHandler,
      tableStateHandler,
      siteHandler,
      sessionFilterHandler,
      soundHandler
    )

  /**
   * Handles a fatal error most likely during action processing
   */
  override def handleFatal(action: Any, e: Throwable): Unit = {
    logger.error(s"Action not handled $action")
    super.handleFatal(action, e)
  }

  /**
   * Handle a non-fatal error, such as dispatching an action with no action handler.
   */
  override def handleError(msg: String): Unit =
    logger.error(s"Action error $msg")

}
