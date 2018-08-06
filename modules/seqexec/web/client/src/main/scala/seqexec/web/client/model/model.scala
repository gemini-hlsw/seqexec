// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import diode.RootModelR
import diode.data._
import gem.Observation
import gem.enum.Site
import monocle.Lens
import monocle.macros.Lenses
import seqexec.model.{ ClientID, Conditions, UserDetails, SequencesQueue, SequenceView, StepId }
import seqexec.model.enum._
import seqexec.model.events._
import seqexec.web.common.{Zipper, FixedLengthBuffer}
import seqexec.web.client.ModelOps._
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

  final case class AvailableTab(id: Option[Observation.Id], status: Option[SequenceState], instrument: Option[Instrument], runningStep: Option[RunningStep], isPreview: Boolean, active: Boolean)
  final case class SequenceTabActive(tab: SequenceTab, active: Boolean)

  object SequenceTabActive {
    implicit val eq: Eq[SequenceTabActive] =
      Eq.by(x => (x.tab, x.active))
    val Empty: SequenceTabActive = SequenceTabActive(SequenceTab.Empty, false)
  }

  sealed trait SequenceTab

  @Lenses
  final case class InstrumentSequenceTab(instrument: Instrument, currentSequence: RefTo[Option[SequenceView]], completedSequence: Option[SequenceView], stepConfigDisplayed: Option[Int]) extends SequenceTab

  object InstrumentSequenceTab {
    implicit val eq: Eq[InstrumentSequenceTab] =
      Eq.by(x => (x.instrument, x.currentSequence, x.completedSequence, x.stepConfigDisplayed))
  }

  @Lenses
  final case class PreviewSequenceTab(currentSequence: RefTo[Option[SequenceView]], stepConfigDisplayed: Option[Int]) extends SequenceTab

  object PreviewSequenceTab {
    implicit val eq: Eq[PreviewSequenceTab] =
      Eq.by(x => (x.currentSequence, x.stepConfigDisplayed))
  }

  object SequenceTab {
    implicit val eq: Eq[SequenceTab] =
      Eq.instance {
        case (a: InstrumentSequenceTab, b: InstrumentSequenceTab) => a === b
        case (a: PreviewSequenceTab, b: PreviewSequenceTab)       => a === b
        case _                                                    => false
      }
    val Empty: SequenceTab = PreviewSequenceTab(RefTo(new RootModelR(None)), None)
    val stepConfigL: Lens[SequenceTab, Option[Int]] = Lens[SequenceTab, Option[Int]] {
      case t: InstrumentSequenceTab => t.stepConfigDisplayed
      case t: PreviewSequenceTab    => t.stepConfigDisplayed
    }(n => a => a match {
      case t: InstrumentSequenceTab => t.copy(stepConfigDisplayed = n)
      case t: PreviewSequenceTab    => t.copy(stepConfigDisplayed = n)
    })
    val currentSequenceL: Lens[SequenceTab, RefTo[Option[SequenceView]]] = Lens[SequenceTab, RefTo[Option[SequenceView]]] {
      case t: InstrumentSequenceTab => t.currentSequence
      case t: PreviewSequenceTab    => t.currentSequence
    }(n => a => a match {
      case t: InstrumentSequenceTab => t.copy(currentSequence = n)
      case t: PreviewSequenceTab    => t.copy(currentSequence = n)
    })
    val completedSequenceL: Lens[SequenceTab, Option[SequenceView]] = Lens[SequenceTab, Option[SequenceView]] {
      case t: InstrumentSequenceTab => t.completedSequence
      case _: PreviewSequenceTab    => None
    }(n => a => a match {
      case t: InstrumentSequenceTab => t.copy(completedSequence = n)
      case t: PreviewSequenceTab    => t
    })
  }

  implicit final class SequenceTabOps(val t: SequenceTab) extends AnyVal {

    def instrument: Option[Instrument] = t match {
      case i: InstrumentSequenceTab => i.instrument.some
      case _                        => none
    }

    def sequence: Option[SequenceView] = t match {
      // Returns the current sequence or if empty the last completed one
      case i: InstrumentSequenceTab => i.currentSequence().orElse(i.completedSequence)
      case i: PreviewSequenceTab    => i.currentSequence()
    }

    def obsId: Option[Observation.Id] = sequence.map(_.id)

    def stepConfigDisplayed: Option[Int] = t match {
      case i: InstrumentSequenceTab => i.stepConfigDisplayed
      case i: PreviewSequenceTab    => i.stepConfigDisplayed
    }

    def isPreview: Boolean = t match {
      case _: InstrumentSequenceTab => false
      case _                        => true
    }

    def runningStep: Option[RunningStep] = t match {
      case _: InstrumentSequenceTab => sequence.flatMap(_.runningStep)
      case _                        => none
    }
  }

  // Model for the tabbed area of sequences
  final case class SequencesOnDisplay(instrumentSequences: Zipper[SequenceTab]) {
    // def withSite(site: Site): SequencesOnDisplay =
    //   SequencesOnDisplay(Zipper.fromNel(site.instruments.map(SequenceTab(_, SequencesOnDisplay.emptySeqRef, None, None))))

    // Display a given step on the focused sequence
    def showStepConfig(i: Int): SequencesOnDisplay =
      copy(instrumentSequences = instrumentSequences.modify(SequenceTab.stepConfigL.set(Some(i))(_)))

    // Don't show steps for the sequence
    def hideStepConfig: SequencesOnDisplay =
      copy(instrumentSequences = instrumentSequences.modify(SequenceTab.stepConfigL.set(None)(_)))

    def focusOnSequence(s: RefTo[Option[SequenceView]]): SequencesOnDisplay = {
      // Replace the sequence for the instrument or the completed sequence and reset displaying a step
      val q = instrumentSequences.findFocus(i => i.sequence === s()).map(_.modify((SequenceTab.currentSequenceL.set(s) andThen SequenceTab.stepConfigL.set(None))(_)))
      copy(instrumentSequences = q.getOrElse(instrumentSequences))
    }

    def focusOnInstrument(i: Instrument): SequencesOnDisplay = {
      // Focus on the instrument
      val q = instrumentSequences.findFocus{
        case t: InstrumentSequenceTab => t.instrument === i
        case _                        => false
      }
      copy(instrumentSequences = q.getOrElse(instrumentSequences))
    }

    def isAnySelected: Boolean = instrumentSequences.exists(_.sequence.isDefined)

    // Is the id on the sequences area?
    def idDisplayed(id: Observation.Id): Boolean =
      instrumentSequences.withFocus.exists { case (s, a) => a && s.sequence.exists(_.id === id) }

    // def instrument(i: Instrument): InstrumentTabActive =
    //   // The getOrElse shouldn't be called as we have an element per instrument
    //   instrumentSequences.withFocus.find(_._1.instrument === i)
    //     .map { case (i, a) => InstrumentTabActive(i, a) }.getOrElse(InstrumentTabActive(SequenceTab.empty, active = false))

    def tab(id: Observation.Id): Option[SequenceTabActive] =
      instrumentSequences.withFocus.find(_._1.obsId.exists(_ === id))
        .map { case (i, a) => SequenceTabActive(i, a) }

    // We'll set the passed SequenceView as completed for the given instruments
    def markCompleted(completed: SequenceView): SequencesOnDisplay = {
      val q = instrumentSequences.findFocus {
        case t: InstrumentSequenceTab => t.instrument === completed.metadata.instrument
        case _                        => false
      }.map(_.modify(SequenceTab.completedSequenceL.set(completed.some)(_)))

      copy(instrumentSequences = q.getOrElse(instrumentSequences))
    }

    def availableTabs: NonEmptyList[AvailableTab] =
      NonEmptyList.fromListUnsafe(instrumentSequences.withFocus.map {
        case (i, a) => AvailableTab(i.sequence.map(_.id), i.sequence.map(_.status), i.instrument, i.runningStep, i.isPreview, a)
      }.toList)
  }

  /**
    * Contains the sequences displayed on the instrument tabs. Note that they are references to sequences on the Queue
    */
  object SequencesOnDisplay {
    val emptySeqRef: RefTo[Option[SequenceView]] = RefTo(new RootModelR(None))

    // We need to initialize the model with some instruments but it will be shortly replaced by the actual list
    val empty: SequencesOnDisplay = SequencesOnDisplay(Zipper.fromNel(NonEmptyList.of(SequenceTab.Empty)))
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
