// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.model

import java.time.LocalTime

import diode.RootModelR
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
  def withSite(site: SeqexecSite): SequencesOnDisplay =
    SequencesOnDisplay(site.instruments.map(SequenceTab(_, SequencesOnDisplay.emptySeqRef, None)).toZipper)

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

  // We need to initialize the model with some instruments but it will be shortly replaced by the actual list
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
                          sequencesOnDisplay: SequencesOnDisplay,
                          firstLoad: Boolean)

object SeqexecUIModel {
  val noSequencesLoaded = SequencesQueue[SequenceView](Conditions.default, None, Nil)
  val initial = SeqexecUIModel(Pages.Root, None, noSequencesLoaded,
    SectionClosed, GlobalLog(Nil), SequencesOnDisplay.empty, true)
}



/**
  * Root of the UI Model of the application
  */
case class SeqexecAppRootModel(ws: WebSocketConnection, site: SeqexecSite, uiModel: SeqexecUIModel)

object SeqexecAppRootModel {
  type LoadedSequences = SequencesQueue[SequenceView]

  val initial = SeqexecAppRootModel(WebSocketConnection.empty, SeqexecSite.SeqexecGS, SeqexecUIModel.initial)
}
