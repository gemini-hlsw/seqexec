package edu.gemini.seqexec.web.client.model

import java.time.LocalTime

import diode.{Action, ActionType, RootModelR}
import diode.data.{Empty, Pot, PotAction, RefTo}
import edu.gemini.seqexec.model.{SeqexecEvent, UserDetails}
import edu.gemini.seqexec.web.common.{Instrument, SeqexecQueue, Sequence}
import org.scalajs.dom.WebSocket

import scalaz._
import Scalaz._

// Actions

// Request loading the queue
case class UpdatedQueue(potResult: Pot[SeqexecQueue]) extends PotAction[SeqexecQueue, UpdatedQueue] {
  override def next(newResult: Pot[SeqexecQueue]) = {
    UpdatedQueue(newResult)
  }
}
// Request a search
case class SearchSequence(criteria: String, potResult: Pot[List[Sequence]] = Empty) extends PotAction[List[Sequence], SearchSequence] {
  override def next(newResult: Pot[List[Sequence]]) = {
    SearchSequence(criteria, newResult)
  }
}

// Actions to close and/open the search area
case object OpenSearchArea extends Action
case object CloseSearchArea extends Action

// Actions to close and/open the dev console area
case object ToggleDevConsole extends Action

// Actions to close and/open the login box
case object OpenLoginBox extends Action
case object CloseLoginBox extends Action

case class LoggedIn(u: UserDetails) extends Action
case object Logout extends Action

// Action to add a sequence to the queue
case class AddToQueue(s: Sequence) extends Action
// Action to remove a sequence from the search results
case class RemoveFromSearch(s: Sequence) extends Action
// Action to select a sequence for display
case class SelectToDisplay(s: Sequence) extends Action

// Actions related to executing sequences
case class RequestRun(s: Sequence) extends Action
case class RequestStop(s: Sequence) extends Action
case class RunStarted(s: Sequence) extends Action
case class RunStopped(s: Sequence) extends Action
case class RunStartFailed(s: Sequence) extends Action
case class RunStopFailed(s: Sequence) extends Action

case class ShowStep(s: Sequence, i: Int) extends Action
case class UnShowStep(s: Sequence) extends Action

case class AppendToLog(s: String) extends Action

// Actions related to web sockets
case object WSConnect extends Action
case class Connecting(ws: WebSocket) extends Action
case object Connected extends Action
case class NewSeqexecEvent(e: SeqexecEvent) extends Action
case class ConnectionError(s: String) extends Action


// End Actions

// UI model
sealed trait SectionVisibilityState
case object SectionOpen extends SectionVisibilityState
case object SectionClosed extends SectionVisibilityState

case class SequenceTab(instrument: Instrument.Instrument, sequence: RefTo[Pot[Sequence]], stepConfigDisplayed: Option[Int])

// Model for the tabbed area of sequences
case class SequencesOnDisplay(instrumentSequences: Zipper[SequenceTab]) {
  // Display a given step on the focused sequence
  def showStep(i: Int):SequencesOnDisplay =
    copy(instrumentSequences.modify(_.copy(stepConfigDisplayed = Some(i))))

  // Don't show steps for the sequence
  def unshowStep:SequencesOnDisplay =
    copy(instrumentSequences.modify(_.copy(stepConfigDisplayed = None)))

  def focusOnSequence(s: RefTo[Pot[Sequence]]):SequencesOnDisplay = {
    // Replace the sequence for the instrument and focus
    val q = instrumentSequences.findZ(i => s().exists(_.instrument === i.instrument)).map(_.modify(_.copy(sequence = s)))
    copy(q | instrumentSequences)
  }
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
  * Contains the sequences displayed on the instrument tabs. Note that they are references to sequences on the Queue
  */
object SequencesOnDisplay {
  val emptySeqRef:RefTo[Pot[Sequence]] = RefTo(new RootModelR[Pot[Sequence]](Empty))

  val empty = SequencesOnDisplay(Instrument.instruments.map(SequenceTab(_, emptySeqRef, None)).toZipper)
}

/**
  * Root of the UI Model of the application
  */
case class SeqexecAppRootModel(ws: Option[WebSocket],
                               user: Option[UserDetails],
                               queue: Pot[SeqexecQueue],
                               searchAreaState: SectionVisibilityState,
                               devConsoleState: SectionVisibilityState,
                               loginBox: SectionVisibilityState,
                               webSocketLog: WebSocketsLog,
                               globalLog: GlobalLog,
                               searchResults: Pot[List[Sequence]],
                               sequencesOnDisplay: SequencesOnDisplay)

object SeqexecAppRootModel {
  val initial = SeqexecAppRootModel(None, None, Empty, SectionClosed, SectionClosed, SectionClosed, WebSocketsLog(Nil), GlobalLog(Nil), Empty, SequencesOnDisplay.empty)
}
