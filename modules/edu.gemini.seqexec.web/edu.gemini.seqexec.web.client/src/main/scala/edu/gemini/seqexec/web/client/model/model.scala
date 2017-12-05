// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client

import diode.RootModelR
import diode.data.{Empty, Pot, RefTo}
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.events.SeqexecEvent.ServerLogMessage
import edu.gemini.web.common.FixedLengthBuffer
import org.scalajs.dom.WebSocket

// import monocle.macros.Lenses

import scalaz._
import Scalaz._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object model {

  // Pages
  object Pages {
    sealed trait SeqexecPages

    case object Root extends SeqexecPages
    final case class InstrumentPage(instrument: Instrument, obsId: Option[SequenceId]) extends SeqexecPages

    implicit val equal: Equal[SeqexecPages] = Equal.equalA
  }

  // UI model
  sealed trait SectionVisibilityState
  case object SectionOpen extends SectionVisibilityState
  case object SectionClosed extends SectionVisibilityState

  object SectionVisibilityState {
    implicit val eq: Equal[SectionVisibilityState] = Equal.equalA[SectionVisibilityState]
  }

  final case class SequenceTab(instrument: Instrument, currentSequence: RefTo[Option[SequenceView]], completedSequence: Option[SequenceView], stepConfigDisplayed: Option[Int]) {
    // Returns the current sequence or if empty the last completed one
    // This must be a def since it will do a call to dereference a RefTo
    def sequence: Option[SequenceView] = currentSequence().orElse(completedSequence)
  }

  object SequenceTab {
    val empty: SequenceTab = SequenceTab(Instrument.F2, RefTo(new RootModelR(None)), None, None)
  }

  // Model for the tabbed area of sequences
  final case class SequencesOnDisplay(instrumentSequences: Zipper[SequenceTab]) {
    def withSite(site: SeqexecSite): SequencesOnDisplay =
      SequencesOnDisplay(site.instruments.map(SequenceTab(_, SequencesOnDisplay.emptySeqRef, None, None)).toZipper)

    // Display a given step on the focused sequence
    def showStep(i: Int): SequencesOnDisplay =
      copy(instrumentSequences = instrumentSequences.modify(_.copy(stepConfigDisplayed = Some(i))))

    // Don't show steps for the sequence
    def unshowStep: SequencesOnDisplay =
      copy(instrumentSequences = instrumentSequences.modify(_.copy(stepConfigDisplayed = None)))

    def focusOnSequence(s: RefTo[Option[SequenceView]]): SequencesOnDisplay = {
      // Replace the sequence for the instrument or the completed sequence
      val q = instrumentSequences.findZ(i => s().exists(_.metadata.instrument === i.instrument)).map(_.modify(_.copy(currentSequence = s)))
      copy(instrumentSequences = q | instrumentSequences)
    }

    def focusOnInstrument(i: Instrument): SequencesOnDisplay = {
      // Focus on the instrument
      val q = instrumentSequences.findZ(s => s.instrument === i)
      copy(instrumentSequences = q | instrumentSequences)
    }

    def isAnySelected: Boolean = instrumentSequences.toStream.exists(_.sequence.isDefined)

    // Is the id on the sequences area?
    def idDisplayed(id: SequenceId): Boolean =
      instrumentSequences.withFocus.toStream.find { case (s, a) => a && s.sequence.exists(_.id === id)}.isDefined

    def instrument(i: Instrument): (SequenceTab, Boolean) =
      // The getOrElse shouldn't be called as we have an element per instrument
      instrumentSequences.withFocus.toStream.find(_._1.instrument === i).getOrElse((SequenceTab.empty, false))

    // We'll set the passed SequenceView as completed for the given instruments
    def markCompleted(completed: SequenceView): SequencesOnDisplay = {
      val q = instrumentSequences.findZ(s => s.instrument === completed.metadata.instrument).map(_.modify(_.copy(completedSequence = completed.some)))
      copy(instrumentSequences = q | instrumentSequences)
    }
  }

  /**
    * Contains the sequences displayed on the instrument tabs. Note that they are references to sequences on the Queue
    */
  object SequencesOnDisplay {
    val emptySeqRef: RefTo[Option[SequenceView]] = RefTo(new RootModelR(None))

    // We need to initialize the model with some instruments but it will be shortly replaced by the actual list
    val empty: SequencesOnDisplay = SequencesOnDisplay(Instrument.gsInstruments.map(SequenceTab(_, emptySeqRef, None, None)).toZipper)
  }

  final case class WebSocketConnection(ws: Pot[WebSocket], nextAttempt: Int, autoReconnect: Boolean)

  object WebSocketConnection {
    val empty: WebSocketConnection = WebSocketConnection(Empty, 0, true)
  }

  /**
    * Keeps a list of log entries for display
    */
  final case class GlobalLog(log: FixedLengthBuffer[ServerLogMessage])

  /**
   * Model to display a resource conflict
   */
  final case class ResourcesConflict(visibility: SectionVisibilityState, id: Option[SequenceId])

  /**
   * UI model, changes here will update the UI
   */
  final case class SeqexecUIModel(navLocation: Pages.SeqexecPages,
                            user: Option[UserDetails],
                            sequences: SeqexecAppRootModel.LoadedSequences,
                            loginBox: SectionVisibilityState,
                            resourceConflict: ResourcesConflict,
                            globalLog: GlobalLog,
                            sequencesOnDisplay: SequencesOnDisplay,
                            firstLoad: Boolean)

  object SeqexecUIModel {
    val noSequencesLoaded: SequencesQueue[SequenceView] = SequencesQueue[SequenceView](Conditions.default, None, Nil)
    val initial: SeqexecUIModel = SeqexecUIModel(Pages.Root, None, noSequencesLoaded,
      SectionClosed, ResourcesConflict(SectionClosed, None), GlobalLog(FixedLengthBuffer.unsafeFromInt(100)), SequencesOnDisplay.empty, true)
  }

  /**
    * Root of the UI Model of the application
    */
  final case class SeqexecAppRootModel(ws: WebSocketConnection, site: Option[SeqexecSite], uiModel: SeqexecUIModel)

  object SeqexecAppRootModel {
    type LoadedSequences = SequencesQueue[SequenceView]

    val initial: SeqexecAppRootModel = SeqexecAppRootModel(WebSocketConnection.empty, None, SeqexecUIModel.initial)
  }
}
