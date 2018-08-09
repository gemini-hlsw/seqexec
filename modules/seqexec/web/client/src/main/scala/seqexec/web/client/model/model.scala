// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats._
import cats.implicits._
import diode.RootModelR
import diode.data._
import gem.Observation
import gem.enum.Site
import seqexec.model.{ Conditions, UserDetails, SequencesQueue, SequenceView }
import seqexec.model.Model._
import seqexec.model.enum._
import seqexec.model.events._
import seqexec.web.client.ModelOps._
import seqexec.web.common.{Zipper, FixedLengthBuffer}
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.QueueTableBody
import org.scalajs.dom.WebSocket
import web.client.table._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object model {
  implicit val eqWebSocket: Eq[WebSocket] =
    Eq.by { x =>
      (x.url, x.protocol, x.readyState)
    }

  implicit def eqRefTo[A: Eq]: Eq[RefTo[A]] =
    Eq.by(_.apply())

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit def eqPot[A: Eq]: Eq[Pot[A]] = Eq.instance {
    case (Empty, Empty)                           => true
    case (Unavailable, Unavailable)               => true
    case (Pending(a), Pending(b))                 => a === b
    case (Ready(a), Ready(b))                     => a === b
    case (PendingStale(a, b), PendingStale(c, d)) => a === c && b === d
    case (Failed(a), Failed(b))                   => a == b
    case (FailedStale(a, b), FailedStale(c, d))   => a === c && b == d
    case _                                        => false
  }

  // Pages
  object Pages {
    sealed trait SeqexecPages extends Product with Serializable

    case object Root extends SeqexecPages
    case object SoundTest extends SeqexecPages
    final case class InstrumentPage(instrument: Instrument) extends SeqexecPages
    final case class SequencePage(instrument: Instrument, obsId: Observation.Id, step: StepId) extends SeqexecPages
    final case class SequenceConfigPage(instrument: Instrument, obsId: Observation.Id, step: Int) extends SeqexecPages

    implicit val equal: Eq[SeqexecPages] = Eq.instance {
      case (Root, Root)                                               => true
      case (SoundTest, SoundTest)                                     => true
      case (InstrumentPage(i), InstrumentPage(j))                     => i === j
      case (SequencePage(i, o, s), SequencePage(j, p, r))             => i === j && o === p && s === r
      case (SequenceConfigPage(i, o, s), SequenceConfigPage(j, p, r)) => i === j && o === p && s === r
      case _                                                          => false
    }
  }

  // UI model
  sealed trait SectionVisibilityState
  case object SectionOpen extends SectionVisibilityState
  case object SectionClosed extends SectionVisibilityState

  object SectionVisibilityState {
    implicit val eq: Eq[SectionVisibilityState] = Eq.fromUniversalEquals
  }

  implicit class SectionVisibilityStateOps(val s: SectionVisibilityState) extends AnyVal {
    def toggle: SectionVisibilityState = s match {
      case SectionOpen   => SectionClosed
      case SectionClosed => SectionOpen
    }
  }

  final case class InstrumentTabActive(tab: SequenceTab, active: Boolean)

  object InstrumentTabActive {
    implicit val eq: Eq[InstrumentTabActive] =
      Eq.by(x => (x.tab, x.active))
  }

  final case class SequenceTab(instrument: Instrument, currentSequence: RefTo[Option[SequenceView]], completedSequence: Option[SequenceView], stepConfigDisplayed: Option[Int]) {
    // Returns the current sequence or if empty the last completed one
    // This must be a def since it will do a call to dereference a RefTo
    def sequence: Option[SequenceView] = currentSequence().orElse(completedSequence)
  }

  object SequenceTab {
    implicit val eq: Eq[SequenceTab] =
      Eq.by(x => (x.instrument, x.currentSequence, x.completedSequence, x.stepConfigDisplayed))
    val empty: SequenceTab = SequenceTab(Instrument.F2, RefTo(new RootModelR(None)), None, None)
  }

  // Model for the tabbed area of sequences
  final case class SequencesOnDisplay(instrumentSequences: Zipper[SequenceTab]) {
    def withSite(site: Site): SequencesOnDisplay =
      SequencesOnDisplay(Zipper.fromNel(site.instruments.map(SequenceTab(_, SequencesOnDisplay.emptySeqRef, None, None))))

    // Display a given step on the focused sequence
    def showStepConfig(i: Int): SequencesOnDisplay =
      copy(instrumentSequences = instrumentSequences.modify(_.copy(stepConfigDisplayed = Some(i))))

    // Don't show steps for the sequence
    def hideStepConfig: SequencesOnDisplay =
      copy(instrumentSequences = instrumentSequences.modify(_.copy(stepConfigDisplayed = None)))

    def focusOnSequence(s: RefTo[Option[SequenceView]]): SequencesOnDisplay = {
      // Replace the sequence for the instrument or the completed sequence and reset displaying a step
      val q = instrumentSequences.findFocus(i => s().exists(_.metadata.instrument === i.instrument)).map(_.modify(_.copy(currentSequence = s, stepConfigDisplayed = None)))
      copy(instrumentSequences = q.getOrElse(instrumentSequences))
    }

    def focusOnInstrument(i: Instrument): SequencesOnDisplay = {
      // Focus on the instrument
      val q = instrumentSequences.findFocus(s => s.instrument === i)
      copy(instrumentSequences = q.getOrElse(instrumentSequences))
    }

    def isAnySelected: Boolean = instrumentSequences.exists(_.sequence.isDefined)

    // Is the id on the sequences area?
    def idDisplayed(id: Observation.Id): Boolean =
      instrumentSequences.withFocus.exists { case (s, a) => a && s.sequence.exists(_.id === id) }

    def instrument(i: Instrument): InstrumentTabActive =
      // The getOrElse shouldn't be called as we have an element per instrument
      instrumentSequences.withFocus.find(_._1.instrument === i)
        .map { case (i, a) => InstrumentTabActive(i, a) }.getOrElse(InstrumentTabActive(SequenceTab.empty, active = false))

    // We'll set the passed SequenceView as completed for the given instruments
    def markCompleted(completed: SequenceView): SequencesOnDisplay = {
      val q = instrumentSequences.findFocus(s => s.instrument === completed.metadata.instrument).map(_.modify(_.copy(completedSequence = completed.some)))
      copy(instrumentSequences = q.getOrElse(instrumentSequences))
    }
  }

  /**
    * Contains the sequences displayed on the instrument tabs. Note that they are references to sequences on the Queue
    */
  object SequencesOnDisplay {
    val emptySeqRef: RefTo[Option[SequenceView]] = RefTo(new RootModelR(None))

    // We need to initialize the model with some instruments but it will be shortly replaced by the actual list
    val empty: SequencesOnDisplay = SequencesOnDisplay(Zipper.fromNel(Instrument.gsInstruments.map(SequenceTab(_, emptySeqRef, None, None))))
  }

  final case class WebSocketConnection(ws: Pot[WebSocket], nextAttempt: Int, autoReconnect: Boolean)

  object WebSocketConnection {
    val empty: WebSocketConnection = WebSocketConnection(Empty, 0, autoReconnect = true)

    implicit val equal: Eq[WebSocketConnection] =
      Eq.by { x =>
        (x.ws, x.nextAttempt, x.autoReconnect)
      }

  }

  /**
    * Keeps a list of log entries for display
    */
  final case class GlobalLog(log: FixedLengthBuffer[ServerLogMessage], display: SectionVisibilityState)

  /**
   * Model to display a resource conflict
   */
  final case class ResourcesConflict(visibility: SectionVisibilityState, id: Option[Observation.Id])

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
                            syncInProgress: Boolean,
                            configTableState: TableState[StepConfigTable.TableColumn],
                            queueTableState: TableState[QueueTableBody.TableColumn],
                            firstLoad: Boolean)

  object SeqexecUIModel {
    val noSequencesLoaded: SequencesQueue[SequenceView] = SequencesQueue[SequenceView](Map.empty, Conditions.Default, None, Nil)
    val initial: SeqexecUIModel = SeqexecUIModel(
      Pages.Root,
      None,
      noSequencesLoaded,
      SectionClosed,
      ResourcesConflict(SectionClosed, None),
      GlobalLog(FixedLengthBuffer.unsafeFromInt(500), SectionClosed),
      SequencesOnDisplay.empty,
      syncInProgress = false,
      StepConfigTable.InitialTableState,
      QueueTableBody.InitialTableState.tableState,
      firstLoad = true)
  }

  /**
    * Root of the UI Model of the application
    */
  final case class SeqexecAppRootModel(ws: WebSocketConnection, site: Option[Site], clientId: Option[ClientID], uiModel: SeqexecUIModel)

  object SeqexecAppRootModel {
    type LoadedSequences = SequencesQueue[SequenceView]

    val initial: SeqexecAppRootModel = SeqexecAppRootModel(WebSocketConnection.empty, None, None, SeqexecUIModel.initial)
  }
}
