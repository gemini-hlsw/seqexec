// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client

import java.util.logging.Logger

import diode.data._
import diode.react.ReactConnector
import diode._
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.handlers._
import edu.gemini.seqexec.web.client.model.SeqexecAppRootModel.LoadedSequences
import edu.gemini.seqexec.web.client.ModelOps._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scalaz.syntax.equal._

object circuit {

  /**
    * Utility class to let components more easily switch parts of the UI depending on the context
    */
  final case class ClientStatus(u: Option[UserDetails], w: WebSocketConnection, anySelected: Boolean) extends UseValueEq {
    def isLogged: Boolean = u.isDefined
    def isConnected: Boolean = w.ws.isReady
  }

  // All these classes are focused views of the root model. They are used to only update small sections of the
  // UI even if other parts of the root model change
  final case class WebSocketsFocus(sequences: LoadedSequences, user: Option[UserDetails], site: SeqexecSite, firstLoad: Boolean) extends UseValueEq
  final case class SequenceInQueue(id: SequenceId, status: SequenceState, instrument: Instrument, active: Boolean, name: String, runningStep: Option[(Int, Int)]) extends UseValueEq
  final case class StatusAndLoadedSequencesFocus(isLogged: Boolean, sequences: List[SequenceInQueue]) extends UseValueEq
  final case class HeaderSideBarFocus(status: ClientStatus, conditions: Conditions, operator: Option[Operator]) extends UseValueEq
  final case class InstrumentStatusFocus(instrument: Instrument, active: Boolean, idState: Option[(SequenceId, SequenceState)], runningStep: Option[(Int, Int)]) extends UseValueEq
  final case class InstrumentTabContentFocus(instrument: Instrument, active: Boolean, sequenceSelected: Boolean) extends UseValueEq
  final case class StatusAndObserverFocus(isLogged: Boolean, name: Option[String], instrument: Instrument, id: Option[SequenceId], observer: Option[Observer]) extends UseValueEq
  final case class StatusAndStepFocus(isLogged: Boolean, instrument: Instrument, stepConfigDisplayed: Option[Int]) extends UseValueEq
  final case class StepsTableFocus(id: SequenceId, instrument: Instrument, state: SequenceState, steps: List[Step], stepConfigDisplayed: Option[Int], nextStepToRun: Option[Int]) extends UseValueEq
  final case class ControlModel(id: SequenceId, isPartiallyExecuted: Boolean, nextStepToRun: Option[Int], status: SequenceState)
  final case class SequenceControlFocus(isLogged: Boolean, isConnected: Boolean, control: Option[ControlModel])

  /**
    * Contains the model for Diode
    */
  object SeqexecCircuit extends Circuit[SeqexecAppRootModel] with ReactConnector[SeqexecAppRootModel] {
    type SearchResults = SequencesQueue[SequenceId]
    private val logger = Logger.getLogger(SeqexecCircuit.getClass.getSimpleName)

    // Model read-writers
    private val webSocketFocusRW: ModelRW[SeqexecAppRootModel, WebSocketsFocus] =
      zoomRW(m => WebSocketsFocus(m.uiModel.sequences, m.uiModel.user, m.site, m.uiModel.firstLoad)) ((m, v) => m.copy(uiModel = m.uiModel.copy(sequences = v.sequences, user = v.user, firstLoad = v.firstLoad), site = v.site))

    private val wsHandler              = new WebSocketHandler(zoomTo(_.ws))
    private val wsEventsHandler        = new WebSocketEventsHandler(webSocketFocusRW)
    private val navigationHandler      = new NavigationHandler(zoomTo(_.uiModel.navLocation))
    private val loginBoxHandler        = new LoginBoxHandler(zoomTo(_.uiModel.loginBox))
    private val resourcesBoxHandler    = new ResourcesBoxHandler(zoomTo(_.uiModel.resourceConflictBox))
    private val userLoginHandler       = new UserLoginHandler(zoomTo(_.uiModel.user))
    private val sequenceDisplayHandler = new SequenceDisplayHandler(zoomRW(m => (m.uiModel.sequencesOnDisplay, m.uiModel.sequences, m.site))((m, v) => m.copy(uiModel = m.uiModel.copy(sequencesOnDisplay = v._1, sequences = v._2), site = v._3)))
    private val sequenceExecHandler    = new SequenceExecutionHandler(zoomTo(_.uiModel.sequences))
    private val globalLogHandler       = new GlobalLogHandler(zoomTo(_.uiModel.globalLog))
    private val conditionsHandler      = new ConditionsHandler(zoomTo(_.uiModel.sequences.conditions))
    private val operatorHandler        = new OperatorHandler(zoomTo(_.uiModel.sequences.operator))

    override protected def initialModel = SeqexecAppRootModel.initial

    // Some useful readers
    val statusAndLoadedSequencesReader: ModelR[SeqexecAppRootModel, StatusAndLoadedSequencesFocus] =
      zoom { c =>
        val sequencesInQueue = c.uiModel.sequences.queue.map { s =>
          val active = c.uiModel.sequencesOnDisplay.idDisplayed(s.id)
          SequenceInQueue(s.id, s.status, s.metadata.instrument, active, s.metadata.name, s.runningStep)
        }
        StatusAndLoadedSequencesFocus(c.uiModel.user.isDefined, sequencesInQueue)
      }

    // Reader for sequences on display
    val headerSideBarReader: ModelR[SeqexecAppRootModel, HeaderSideBarFocus] =
      zoom(c => HeaderSideBarFocus(ClientStatus(c.uiModel.user, c.ws, c.uiModel.sequencesOnDisplay.isAnySelected), c.uiModel.sequences.conditions, c.uiModel.sequences.operator))

    def instrumentStatusReader(i: Instrument): ModelR[SeqexecAppRootModel, InstrumentStatusFocus] =
      zoom(_.uiModel.sequencesOnDisplay.instrument(i)).zoom {
        case (tab, active) => InstrumentStatusFocus(tab.instrument, active, tab.sequence.map(s => (s.id, s.status)), tab.sequence.flatMap(_.runningStep))
      }

    def instrumentTabContentReader(i: Instrument): ModelR[SeqexecAppRootModel, InstrumentTabContentFocus] =
      zoom(_.uiModel.sequencesOnDisplay.instrument(i)).zoom {
        case (tab, active) => InstrumentTabContentFocus(tab.instrument, active, tab.sequence.isDefined)
      }

    private def instrumentTab(i: Instrument): ModelR[SeqexecAppRootModel, (SequenceTab, Boolean)] = zoom(_.uiModel.sequencesOnDisplay.instrument(i))

    def sequenceObserverReader(i: Instrument): ModelR[SeqexecAppRootModel, StatusAndObserverFocus] =
      statusReader.zip(instrumentTab(i)).zoom {
        case (status, (tab, _)) => StatusAndObserverFocus(status.isLogged, tab.sequence.map(_.metadata.name), i, tab.sequence.map(_.id), tab.sequence.flatMap(_.metadata.observer))
      }

    def statusAndStepReader(i: Instrument): ModelR[SeqexecAppRootModel, StatusAndStepFocus] =
      statusReader.zip(instrumentTab(i)).zoom {
        case (status, (tab, _)) => StatusAndStepFocus(status.isLogged, i, tab.stepConfigDisplayed)
      }

    def stepsTableReader(i: Instrument): ModelR[SeqexecAppRootModel, (ClientStatus, Option[StepsTableFocus])] =
      statusReader.zip(instrumentTab(i)).zoom {
        case (status, (tab, _)) =>
          (status, tab.sequence.map { sequence =>
            StepsTableFocus(sequence.id, i, sequence.status, sequence.steps, tab.stepConfigDisplayed, sequence.nextStepToRun)
          })
      }

    def sequenceControlReader(i: Instrument): ModelR[SeqexecAppRootModel, SequenceControlFocus] =
      statusReader.zip(instrumentTab(i)).zoom {
        case (status, (tab, _)) =>
          SequenceControlFocus(status.isLogged, status.isConnected, tab.sequence.map(s => ControlModel(s.id, s.isPartiallyExecuted, s.nextStepToRun, s.status)))
      }

    // Reader for a specific sequence if available
    def sequenceReader(id: SequenceId): ModelR[_, Option[SequenceView]] =
      zoom(_.uiModel.sequences.queue.find(_.id === id))

    // Reader to indicate the allowed interactions
    val statusReader: ModelR[SeqexecAppRootModel, ClientStatus] = zoom(m => ClientStatus(m.uiModel.user, m.ws, m.uiModel.sequencesOnDisplay.isAnySelected))

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
      resourcesBoxHandler,
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
    }

  }
}
