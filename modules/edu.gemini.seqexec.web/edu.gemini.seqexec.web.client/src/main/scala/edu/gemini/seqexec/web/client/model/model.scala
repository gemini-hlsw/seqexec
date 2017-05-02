package edu.gemini.seqexec.web.client.model

import java.time.LocalTime

import diode.{Action, RootModelR}
import diode.data.{Empty, Pot, RefTo}
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.model.Model._
import org.scalajs.dom.WebSocket

import scalaz._
import Scalaz._

// Actions

// Actions to close and/open the dev console area
case object ToggleDevConsole extends Action

// Actions to close and/open the login box
case object OpenLoginBox extends Action
case object CloseLoginBox extends Action

case class LoggedIn(u: UserDetails) extends Action
case object Logout extends Action

// Action to select a sequence for display
case class SelectToDisplay(s: SequenceView) extends Action

// Actions related to executing sequences
case class RequestRun(s: SequenceView) extends Action
case class RequestPause(s: SequenceView) extends Action
case class RunStarted(s: SequenceView) extends Action
case class RunPaused(s: SequenceView) extends Action
case class RunStartFailed(s: SequenceView) extends Action
case class RunPauseFailed(s: SequenceView) extends Action

case class ShowStep(s: SequenceView, i: Int) extends Action
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
case class FlipSkipStep(view: SequenceView, step: Step) extends Action
case class FlipBreakpointStep(view: SequenceView, step: Step) extends Action
case class UpdateObserver(view: SequenceView, name: String) extends Action
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

case class SequenceTab(instrument: Instrument, sequence: RefTo[Option[SequenceView]], stepConfigDisplayed: Option[Int])

/**
  * Internal list of object names.
  * TODO This should belong to the model
  */
object InstrumentNames {
  val instruments = NonEmptyList[Instrument]("Flamingos2", "GMOS-S", "GPI", "GSAOI")
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

  def currentSequences: Map[Instrument, Option[SequenceView]] =
    instrumentSequences.map(tab => tab.instrument -> tab.sequence()).toStream.toMap
}

/**
  * Contains the sequences displayed on the instrument tabs. Note that they are references to sequences on the Queue
  */
object SequencesOnDisplay {
  val emptySeqRef: RefTo[Option[SequenceView]] = RefTo(new RootModelR(None))

  val empty = SequencesOnDisplay(InstrumentNames.instruments.map(SequenceTab(_, emptySeqRef, None)).toZipper)
}

case class WebSocketConnection(ws: Pot[WebSocket], nextAttempt: Int)

object WebSocketConnection {
  val empty = WebSocketConnection(Empty, 0)
}

case class WebSocketsLog(log: List[SeqexecEvent]) {
  // Upper bound of accepted events or we may run out of memory
  val maxLength = 100
  def append(e: SeqexecEvent):WebSocketsLog = copy((log :+ e).take(maxLength - 1))
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
  * Root of the UI Model of the application
  */
case class SeqexecAppRootModel(ws: WebSocketConnection,
                               user: Option[UserDetails],
                               sequences: SeqexecAppRootModel.LoadedSequences,
                               devConsoleState: SectionVisibilityState,
                               loginBox: SectionVisibilityState,
                               webSocketLog: WebSocketsLog,
                               globalLog: GlobalLog,
                               searchResults: Pot[SequencesQueue[SequenceId]],
                               sequencesOnDisplay: SequencesOnDisplay)

object SeqexecAppRootModel {
  type LoadedSequences = SequencesQueue[SequenceView]
  val noSequencesLoaded = SequencesQueue[SequenceView](Conditions.default, None, Nil)

  val initial = SeqexecAppRootModel(WebSocketConnection.empty, None, noSequencesLoaded,
    SectionClosed, SectionClosed, WebSocketsLog(Nil), GlobalLog(Nil), Empty, SequencesOnDisplay.empty)
}
