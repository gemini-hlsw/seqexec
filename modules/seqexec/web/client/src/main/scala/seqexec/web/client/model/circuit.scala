// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.{Eq, Order}
import cats.implicits._
import diode._
import diode.data._
import diode.react.ReactConnector
import gem.Observation
import gem.enum.Site
import java.util.logging.Logger
import japgolly.scalajs.react.Callback
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.events._
import seqexec.web.client.model._
import seqexec.web.client.model.SeqexecAppRootModel.LoadedSequences
import seqexec.web.client.lenses._
import seqexec.web.client.handlers._
import seqexec.web.client.ModelOps._
import seqexec.web.client.actions.{AppendToLog, CloseLoginBox, CloseResourcesBox, OpenLoginBox, OpenResourcesBox, ServerMessage, show}
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.QueueTableBody
import web.client.table._

object circuit {
  /**
    * Utility class to let components more easily switch parts of the UI depending on the context
    */
  final case class ClientStatus(u: Option[UserDetails], w: WebSocketConnection, anySelected: Boolean, syncInProgress: Boolean) extends UseValueEq {
    def isLogged: Boolean = u.isDefined
    def isConnected: Boolean = w.ws.isReady
  }

  object ClientStatus {
    implicit val eq: Eq[ClientStatus] =
      Eq.by (x => (x.u, x.w, x.anySelected, x.syncInProgress))
  }

  // All these classes are focused views of the root model. They are used to only update small sections of the
  // UI even if other parts of the root model change
  final case class WebSocketsFocus(location: Pages.SeqexecPages, sequences: LoadedSequences, user: Option[UserDetails], clientId: Option[ClientID], site: Option[Site]) extends UseValueEq
  final case class InitialSyncFocus(location: Pages.SeqexecPages, sod: SequencesOnDisplay, firstLoad: Boolean) extends UseValueEq
  final case class SequenceInQueue(id: Observation.Id, status: SequenceState, instrument: Instrument, active: Boolean, name: String, targetName: Option[TargetName], runningStep: Option[RunningStep]) extends UseValueEq
  object SequenceInQueue {
    implicit val order: Order[SequenceInQueue] = Order.by(_.id)
    implicit val ordering: scala.math.Ordering[SequenceInQueue] = order.toOrdering
  }
  final case class StatusAndLoadedSequencesFocus(isLogged: Boolean, sequences: List[SequenceInQueue], tableState: TableState[QueueTableBody.TableColumn]) extends UseValueEq
  final case class HeaderSideBarFocus(status: ClientStatus, conditions: Conditions, operator: Option[Operator]) extends UseValueEq
  final case class InstrumentStatusFocus(instrument: Instrument, active: Boolean, idState: Option[(Observation.Id, SequenceState)], runningStep: Option[RunningStep]) extends UseValueEq
  final case class InstrumentTabContentFocus(instrument: Instrument, active: Boolean, sequenceSelected: Boolean, logDisplayed: SectionVisibilityState) extends UseValueEq
  final case class StatusAndObserverFocus(isLogged: Boolean, name: Option[String], instrument: Instrument, id: Option[Observation.Id], observer: Option[Observer], status: Option[SequenceState], targetName: Option[TargetName]) extends UseValueEq
  final case class StatusAndStepFocus(isLogged: Boolean, instrument: Instrument, id: Option[Observation.Id], stepConfigDisplayed: Option[Int], totalSteps: Int) extends UseValueEq
  final case class StepsTableFocus(id: Observation.Id, instrument: Instrument, state: SequenceState, steps: List[Step], stepConfigDisplayed: Option[Int], nextStepToRun: Option[Int]) extends UseValueEq
  final case class StepsTableAndStatusFocus(status: ClientStatus, stepsTable: Option[StepsTableFocus], configTableState: TableState[StepConfigTable.TableColumn]) extends UseValueEq
  final case class ControlModel(id: Observation.Id, isPartiallyExecuted: Boolean, nextStepToRun: Option[Int], status: SequenceState, inConflict: Boolean) extends UseValueEq
  final case class SequenceControlFocus(isLogged: Boolean, isConnected: Boolean, control: Option[ControlModel], syncInProgress: Boolean) extends UseValueEq
  final case class TableStates(queueTable: TableState[QueueTableBody.TableColumn], stepConfigTable: TableState[StepConfigTable.TableColumn]) extends UseValueEq

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
    * Contains the model for Diode
    */
  object SeqexecCircuit extends Circuit[SeqexecAppRootModel] with ReactConnector[SeqexecAppRootModel] {
    type SearchResults = SequencesQueue[Observation.Id]
    private val logger = Logger.getLogger(SeqexecCircuit.getClass.getSimpleName)
    addProcessor(new LoggingProcessor[SeqexecAppRootModel]())

    implicit object ClientStatusEq extends FastEq[ClientStatus] {
      override def eqv(a: ClientStatus, b: ClientStatus): Boolean = a === b
    }

    implicit object InstrumentTabActiveEq extends FastEq[InstrumentTabActive] {
      override def eqv(a: InstrumentTabActive, b: InstrumentTabActive): Boolean = a === b
    }

    implicit object SequenceTabEq extends FastEq[SequenceTab] {
      override def eqv(a: SequenceTab, b: SequenceTab): Boolean = a === b
    }

    def dispatchCB[A <: Action](a: A): Callback = Callback(dispatch(a))

    // Model read-writers
    val webSocketFocusRW: ModelRW[SeqexecAppRootModel, WebSocketsFocus] =
      zoomRW(m => WebSocketsFocus(m.uiModel.navLocation, m.uiModel.sequences, m.uiModel.user, m.clientId, m.site)) ((m, v) => m.copy(uiModel = m.uiModel.copy(sequences = v.sequences, user = v.user), clientId = v.clientId, site = v.site))

    val initialSyncFocusRW: ModelRW[SeqexecAppRootModel, InitialSyncFocus] =
      zoomRW(m => InitialSyncFocus(m.uiModel.navLocation, m.uiModel.sequencesOnDisplay, m.uiModel.firstLoad)) ((m, v) => m.copy(uiModel = m.uiModel.copy(navLocation = v.location, sequencesOnDisplay = v.sod, firstLoad = v.firstLoad)))

    val tableStateRW: ModelRW[SeqexecAppRootModel, TableStates] =
      zoomRW(m => TableStates(m.uiModel.queueTableState, m.uiModel.configTableState)) ((m, v) => m.copy(uiModel = m.uiModel.copy(queueTableState = v.queueTable, configTableState = v.stepConfigTable)))

    private val wsHandler                = new WebSocketHandler(zoomTo(_.ws))
    private val serverMessagesHandler    = new ServerMessagesHandler(webSocketFocusRW)
    private val initialSyncHandler       = new InitialSyncHandler(initialSyncFocusRW)
    private val navigationHandler        = new NavigationHandler(zoomTo(_.uiModel.navLocation))
    private val loginBoxHandler          = new ModalBoxHandler(OpenLoginBox, CloseLoginBox, zoomTo(_.uiModel.loginBox))
    private val resourcesBoxHandler      = new ModalBoxHandler(OpenResourcesBox, CloseResourcesBox, zoomTo(_.uiModel.resourceConflict.visibility))
    private val userLoginHandler         = new UserLoginHandler(zoomTo(_.uiModel.user))
    private val sequenceDisplayHandler   = new SequenceDisplayHandler(zoomRW(m => (m.uiModel.sequencesOnDisplay, m.site))((m, v) => m.copy(uiModel = m.uiModel.copy(sequencesOnDisplay = v._1), site = v._2)))
    private val sequenceExecHandler      = new SequenceExecutionHandler(zoomTo(_.uiModel.sequences))
    private val resourcesConflictHandler = new SequenceInConflictHandler(zoomTo(_.uiModel.resourceConflict.id))
    private val globalLogHandler         = new GlobalLogHandler(zoomTo(_.uiModel.globalLog))
    private val conditionsHandler        = new ConditionsHandler(zoomTo(_.uiModel.sequences.conditions))
    private val operatorHandler          = new OperatorHandler(zoomTo(_.uiModel.sequences.operator))
    private val syncToAddedHandler       = new SyncToAddedRemovedRunHandler(zoomTo(_.uiModel.navLocation))
    private val remoteRequestsHandler    = new RemoteRequestsHandler(zoomTo(_.clientId))
    private val syncRequestsHandler      = new SyncRequestsHandler(zoomTo(_.uiModel.syncInProgress))
    private val debuggingHandler         = new DebuggingHandler(zoomTo(_.uiModel.sequences))
    private val stepConfigStateHandler   = new StepConfigTableStateHandler(tableStateRW)

    override protected def initialModel = SeqexecAppRootModel.initial

    // Some useful readers
    val statusAndLoadedSequencesReader: ModelR[SeqexecAppRootModel, StatusAndLoadedSequencesFocus] =
      zoom { c =>
        val sequencesInQueue = c.uiModel.sequences.queue.map { s =>
          val active = c.uiModel.sequencesOnDisplay.idDisplayed(s.id)
          val targetName = firstScienceStepTargetNameT.headOption(s)
          SequenceInQueue(s.id, s.status, s.metadata.instrument, active, s.metadata.name, targetName, s.runningStep)
        }
        StatusAndLoadedSequencesFocus(c.uiModel.user.isDefined, sequencesInQueue.sorted, c.uiModel.queueTableState)
      }

    // Reader to indicate the allowed interactions
    val statusReader: ModelR[SeqexecAppRootModel, ClientStatus] = zoom(m => ClientStatus(m.uiModel.user, m.ws, m.uiModel.sequencesOnDisplay.isAnySelected, m.uiModel.syncInProgress))

    // Reader to contain the sequence in conflict
    val sequenceInConflictReader: ModelR[SeqexecAppRootModel, Option[Observation.Id]] = zoomTo(_.uiModel.resourceConflict.id)

    // Reader for sequences on display
    val headerSideBarReader: ModelR[SeqexecAppRootModel, HeaderSideBarFocus] =
      zoom(c => HeaderSideBarFocus(ClientStatus(c.uiModel.user, c.ws, c.uiModel.sequencesOnDisplay.isAnySelected, c.uiModel.syncInProgress), c.uiModel.sequences.conditions, c.uiModel.sequences.operator))

    val logDisplayedReader: ModelR[SeqexecAppRootModel, SectionVisibilityState] =
      zoom(_.uiModel.globalLog.display)

    def instrumentTab(i: Instrument): ModelR[SeqexecAppRootModel, InstrumentTabActive] =
      zoom(_.uiModel.sequencesOnDisplay.instrument(i))

    def instrumentStatusReader(i: Instrument): ModelR[SeqexecAppRootModel, InstrumentStatusFocus] =
      instrumentTab(i).zoom {
        case InstrumentTabActive(tab, active) => InstrumentStatusFocus(tab.instrument, active, tab.sequence.map(s => (s.id, s.status)), tab.sequence.flatMap(_.runningStep))
      }

    def instrumentTabContentReader(i: Instrument): ModelR[SeqexecAppRootModel, InstrumentTabContentFocus] =
      logDisplayedReader.zip(instrumentTab(i)).zoom {
        case (log, InstrumentTabActive(tab, active)) => InstrumentTabContentFocus(tab.instrument, active, tab.sequence.isDefined, log)
      }

    def sequenceObserverReader(i: Instrument): ModelR[SeqexecAppRootModel, StatusAndObserverFocus] =
      statusReader.zip(instrumentTab(i)).zoom {
        case (status, InstrumentTabActive(tab, _)) =>
          val targetName = tab.sequence.flatMap(firstScienceStepTargetNameT.headOption)
          StatusAndObserverFocus(status.isLogged, tab.sequence.map(_.metadata.name), i, tab.sequence.map(_.id), tab.sequence.flatMap(_.metadata.observer), tab.sequence.map(_.status), targetName)
      }

    def statusAndStepReader(i: Instrument): ModelR[SeqexecAppRootModel, StatusAndStepFocus] =
      statusReader.zip(instrumentTab(i)).zoom {
        case (status, InstrumentTabActive(tab, _)) =>
        StatusAndStepFocus(status.isLogged, i, tab.sequence.map(_.id), tab.stepConfigDisplayed, tab.sequence.foldMap(_.steps.length))
      }

    def stepsTableReaderF(i: Instrument): ModelR[SeqexecAppRootModel, Option[StepsTableFocus]] =
      instrumentTab(i).zoom {
        case InstrumentTabActive(tab, _) =>
          tab.sequence.map { sequence =>
            StepsTableFocus(sequence.id, i, sequence.status, sequence.steps, tab.stepConfigDisplayed, sequence.nextStepToRun)
          }
      }

    def stepsTableReader(i: Instrument): ModelR[SeqexecAppRootModel, StepsTableAndStatusFocus] =
      statusReader.zip(stepsTableReaderF(i)).zip(configTableState).zoom {
        case ((s, f), t) => StepsTableAndStatusFocus(s, f, t)
      }

    def sequenceControlReader(i: Instrument): ModelR[SeqexecAppRootModel, SequenceControlFocus] =
      sequenceInConflictReader.zip(statusReader.zip(instrumentTab(i))).zoom {
        case (inConflict, (status, InstrumentTabActive(tab, _))) =>
          SequenceControlFocus(status.isLogged, status.isConnected, tab.sequence.map(s => ControlModel(s.id, s.isPartiallyExecuted, s.nextStepToRun, s.status, inConflict.exists(_ === s.id))), status.syncInProgress)
      }

    // Reader for a specific sequence if available
    def sequenceReader(id: Observation.Id): ModelR[_, Option[SequenceView]] =
      zoom(_.uiModel.sequences.queue.find(_.id === id))

    val configTableState: ModelR[SeqexecAppRootModel, TableState[StepConfigTable.TableColumn]] =
      zoom(_.uiModel.configTableState)

    /**
      * Makes a reference to a sequence on the queue.
      * This way we have a normalized model and need to update it in only one place
      */
    def sequenceRef(id: Observation.Id): RefTo[Option[SequenceView]] =
      RefTo(sequenceReader(id))

    override protected def actionHandler = composeHandlers(
      wsHandler,
      foldHandlers(serverMessagesHandler, syncToAddedHandler, initialSyncHandler),
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
      stepConfigStateHandler)

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
}
