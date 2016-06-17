package edu.gemini.seqexec.web.client.model

import java.time.LocalTime

import diode.RootModelR
import diode.data.{Empty, Pot, PotAction, RefTo}
import edu.gemini.seqexec.model.{SeqexecEvent, UserDetails}
import edu.gemini.seqexec.web.common.{Instrument, SeqexecQueue, Sequence}

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
case object OpenSearchArea
case object CloseSearchArea

// Actions to close and/open the dev console area
case object ToggleDevConsole

// Actions to close and/open the login box
case object OpenLoginBox
case object CloseLoginBox

case class LoggedIn(u: UserDetails)
case object Logout

// Action to add a sequence to the queue
case class AddToQueue(s: Sequence)
// Action to remove a sequence from the search results
case class RemoveFromSearch(s: Sequence)
// Action to select a sequence for display
case class SelectToDisplay(s: Sequence)

// Actions related to web sockets
case object ConnectionOpened
case object ConnectionClosed
case class NewSeqexecEvent(e: SeqexecEvent)
case class ConnectionError(s: String)

// Actions related to executing sequences
case class RequestRun(s: Sequence)
case class RequestStop(s: Sequence)
case class RunStarted(s: Sequence)
case class RunStopped(s: Sequence)
case class RunStartFailed(s: Sequence)
case class RunStopFailed(s: Sequence)

case class ShowStep(s: Sequence, i: Int)
case class UnShowStep(s: Sequence)

case class AppendToLog(s: String)

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
case class SeqexecAppRootModel(user: Option[UserDetails],
                               queue: Pot[SeqexecQueue],
                               searchAreaState: SectionVisibilityState,
                               devConsoleState: SectionVisibilityState,
                               loginBox: SectionVisibilityState,
                               webSocketLog: WebSocketsLog,
                               globalLog: GlobalLog,
                               searchResults: Pot[List[Sequence]],
                               sequencesOnDisplay: SequencesOnDisplay)

object SeqexecAppRootModel {
  val initial = SeqexecAppRootModel(None, Empty, SectionClosed, SectionClosed, SectionClosed, WebSocketsLog(Nil), GlobalLog(Nil), Empty, SequencesOnDisplay.empty)
}
