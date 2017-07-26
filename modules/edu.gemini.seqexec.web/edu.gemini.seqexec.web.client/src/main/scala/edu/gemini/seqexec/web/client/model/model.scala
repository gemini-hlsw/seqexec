package edu.gemini.seqexec.web.client.model

import java.time.LocalTime

import diode.{Action, RootModelR}
import diode.data.{Empty, Pot, RefTo}
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.model.Model._
import org.scalajs.dom.WebSocket

import scalaz._
import Scalaz._

// Pages
object Pages {
  sealed trait SeqexecPages

  case object Root extends SeqexecPages
  case class InstrumentPage(instrument: Instrument, obsId: Option[SequenceId]) extends SeqexecPages
}

// Actions
case class NavigateTo(page: Pages.SeqexecPages) extends Action
case class NavigateSilentTo(page: Pages.SeqexecPages) extends Action
case class SyncToPage(view: SequenceView) extends Action

// Actions to close and/open the login box
case object OpenLoginBox extends Action
case object CloseLoginBox extends Action

case class LoggedIn(u: UserDetails) extends Action
case object Logout extends Action

// Action to select a sequence for display
case class SelectToDisplay(s: SequenceView) extends Action
case class SelectIdToDisplay(id: SequenceId) extends Action
case class SelectInstrumentToDisplay(i: Instrument) extends Action

// Actions related to executing sequences
case class RequestRun(s: SequenceView) extends Action
case class RequestSync(s: SequenceView) extends Action
case class RequestPause(s: SequenceView) extends Action
case class RunStarted(s: SequenceView) extends Action
case class RunPaused(s: SequenceView) extends Action
case class RunSync(s: SequenceView) extends Action
case class RunStartFailed(s: SequenceView) extends Action
case class RunPauseFailed(s: SequenceView) extends Action
case class RunSyncFailed(s: SequenceView) extends Action

case class ShowStep(s: SequenceId, i: Int) extends Action
case class UnShowStep(s: SequenceView) extends Action

case class AppendToLog(s: String) extends Action

// Actions related to web sockets
case class WSConnect(delay: Int) extends Action
case object Connecting extends Action
case class Connected(ws: WebSocket, delay: Int) extends Action
case class ConnectionClosed(delay: Int) extends Action
case class ConnectionError(s: String) extends Action
case class ServerMessage(e: SeqexecEvent) extends Action

// Temporal actions for UI prototyping
case class FlipSkipStep(id: SequenceId, step: Step) extends Action
case class FlipBreakpointStep(id: SequenceId, step: Step) extends Action
case class UpdateObserver(id: SequenceId, name: String) extends Action
case class UpdateOperator(name: String) extends Action
case class UpdateImageQuality(iq: ImageQuality) extends Action
case class UpdateCloudCover(cc: CloudCover) extends Action
case class UpdateSkyBackground(sb: SkyBackground) extends Action
case class UpdateWaterVapor(wv: WaterVapor) extends Action

// End Actions

// UI model
sealed trait SectionVisibilityState
case object SectionOpen extends SectionVisibilityState
case object SectionClosed extends SectionVisibilityState

object SectionVisibilityState {
  implicit val eq = Equal.equalA[SectionVisibilityState]
}

case class SequenceTab(instrument: Instrument, sequence: RefTo[Option[SequenceView]], stepConfigDisplayed: Option[Int])

object SequenceTab {
  val empty = SequenceTab(F2, RefTo(new RootModelR(None)), None)
}

// Model for the tabbed area of sequences
case class SequencesOnDisplay(instrumentSequences: Zipper[SequenceTab]) {
  // Display a given step on the focused sequence
  def showStep(i: Int): SequencesOnDisplay =
    copy(instrumentSequences = instrumentSequences.modify(_.copy(stepConfigDisplayed = Some(i))))

  // Don't show steps for the sequence
  def unshowStep: SequencesOnDisplay =
    copy(instrumentSequences = instrumentSequences.modify(_.copy(stepConfigDisplayed = None)))

  def focusOnSequence(s: RefTo[Option[SequenceView]]): SequencesOnDisplay = {
    // Replace the sequence for the instrument and focus
    val q = instrumentSequences.findZ(i => s().exists(_.metadata.instrument === i.instrument)).map(_.modify(_.copy(sequence = s)))
    copy(instrumentSequences = q | instrumentSequences)
  }

  def focusOnInstrument(i: Instrument): SequencesOnDisplay = {
    // Focus on the instrument
    val q = instrumentSequences.findZ(s => s.instrument === i)
    copy(instrumentSequences = q | instrumentSequences)
  }

  def isAnySelected: Boolean = instrumentSequences.toStream.exists(_.sequence().isDefined)

  // Is the id on the sequences area?
  def idDisplayed(id: SequenceId): Boolean =
    instrumentSequences.withFocus.toStream.find { case (s, a) => a && s.sequence().exists(_.id === id)}.isDefined

  def instrument(i: Instrument): (SequenceTab, Boolean) =
    // The getOrElse shouldn't be called as we have an element per instrument
    instrumentSequences.withFocus.toStream.find(_._1.instrument === i).getOrElse((SequenceTab.empty, false))
}

/**
  * Contains the sequences displayed on the instrument tabs. Note that they are references to sequences on the Queue
  */
object SequencesOnDisplay {
  val emptySeqRef: RefTo[Option[SequenceView]] = RefTo(new RootModelR(None))

  val empty = SequencesOnDisplay(Instrument.gsInstruments.map(SequenceTab(_, emptySeqRef, None)).toZipper)
}

case class WebSocketConnection(ws: Pot[WebSocket], nextAttempt: Int)

object WebSocketConnection {
  val empty = WebSocketConnection(Empty, 0)
}

case class GlobalLogEntry(timestamp: LocalTime, s: String)

/**
  * Keeps a list of log entries for display
  */
case class GlobalLog(log: List[GlobalLogEntry]) {
  // Upper bound of accepted events or we may run out of memory
  val maxLength = 500
  def append(e: String):GlobalLog =
    copy((log :+ GlobalLogEntry(LocalTime.now(), e)).take(maxLength - 1))
}

/**
 * UI model, changes here will update the UI
 */
case class SeqexecUIModel(navLocation: Pages.SeqexecPages,
                          user: Option[UserDetails],
                          sequences: SeqexecAppRootModel.LoadedSequences,
                          loginBox: SectionVisibilityState,
                          globalLog: GlobalLog,
                          sequencesOnDisplay: SequencesOnDisplay)

object SeqexecUIModel {
  val noSequencesLoaded = SequencesQueue[SequenceView](Conditions.default, None, Nil)
  val initial = SeqexecUIModel(Pages.Root, None, noSequencesLoaded,
    SectionClosed, GlobalLog(Nil), SequencesOnDisplay.empty)
}



/**
  * Root of the UI Model of the application
  */
case class SeqexecAppRootModel(ws: WebSocketConnection, uiModel: SeqexecUIModel)

object SeqexecAppRootModel {
  type LoadedSequences = SequencesQueue[SequenceView]

  val initial = SeqexecAppRootModel(WebSocketConnection.empty, SeqexecUIModel.initial)
}
