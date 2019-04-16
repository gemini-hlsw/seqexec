// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import gem.Observation
import monocle.Getter
import monocle.Optional
import monocle.Traversal
import monocle.macros.Lenses
import monocle.std
import shapeless.tag
import seqexec.model.Observer
import seqexec.model.SequenceView
import seqexec.model.SequencesQueue
import seqexec.model.StepId
import seqexec.model.CalibrationQueueId
import seqexec.model.enum._
import seqexec.web.common.Zipper
import seqexec.web.client.circuit.SequenceObserverFocus
import seqexec.web.client.circuit.DayCalObserverFocus
import seqexec.web.client.components.sequence.steps.StepsTable
import seqexec.web.client.model.ModelOps._
import web.client.table._

// Model for the tabbed area of sequences
@Lenses
final case class SequencesOnDisplay(tabs: Zipper[SeqexecTab]) {
  private def isOnFocus(id: Observation.Id): Boolean =
    SequencesOnDisplay.focusSequence
      .getOption(this)
      .exists(_.obsId === id)

  // Display a given step on the focused sequence
  def showStepConfig(id: Observation.Id, i: Int): SequencesOnDisplay =
    if (isOnFocus(id)) {
      SequencesOnDisplay.tabs.modify(
        _.modify(SequencesOnDisplay.stepL.set(i.some)))(this)
    } else {
      this
    }

  // Don't show steps for the sequence
  def hideStepConfig: SequencesOnDisplay =
    SequencesOnDisplay.tabs.modify(
      _.modify(SequencesOnDisplay.stepL.set(None)))(this)

  // Focus on a tab for the instrument and id
  def focusOnSequence(inst: Instrument,
                      id:   Observation.Id): SequencesOnDisplay = {
    val q = tabs.findFocusP {
      case t: SequenceTab =>
        t.sequence.s.id === id && t.sequence.metadata.instrument === inst
    }
    copy(tabs = q.getOrElse(tabs))
  }

  /**
    * List of loaded sequence ids
    */
  def loadedIds: List[Observation.Id] =
    tabs.toNel.collect {
      case InstrumentSequenceTab(_, Right(curr), _, _, _, _) => curr.id
    }

  /**
    * Update the observer for the cal tab
    */
  def updateCalTabObserver(o: Observer): SequencesOnDisplay = {
    val q = tabs.map {
      case c @ CalibrationQueueTab(_, _, _) =>
        CalibrationQueueTab.observer.set(o.some)(c)
      case i =>
        i
    }
    copy(tabs = q)
  }

  /**
    * Replace the tabs when the core model is updated
    */
  def updateFromQueue(s: SequencesQueue[SequenceView]): SequencesOnDisplay = {
    // Sequences in view stored after completion
    val completedIds: List[Option[SequenceView]] =
      SequencesOnDisplay.completedTabs
        .getAll(this)
        .filterNot(x => s.loaded.values.toList.contains(x.id))
        .map(_.some)
    val allIds = s.sessionQueue.map(_.id)
    val loadedInSessionQueue = s.loaded.values.toList.map { id =>
      s.sessionQueue.find(_.id === id)
    }
    val updated =
      updateLoaded(loadedInSessionQueue ::: completedIds, allIds).tabs
        .map {
          case p @ PreviewSequenceTab(curr, r, _, t, o) =>
            s.sessionQueue
              .find(_.id === curr.id)
              .map(s => PreviewSequenceTab(s, r, false, t, o))
              .getOrElse(p)
          case c @ CalibrationQueueTab(_, _, _) =>
            s.queues
              .get(CalibrationQueueId)
              .map { q =>
                val t = q.cmdState match {
                  case BatchCommandState.Run(o, _, _) =>
                    CalibrationQueueTab.observer.set(o.some)(c)
                  case _ =>
                    c
                }
                CalibrationQueueTab.state.set(q.execState)(t)
              }
              .getOrElse(c)
          case t => t
        }
    SequencesOnDisplay(updated)
  }

  /**
    * Replace the list of loaded sequences
    */
  private def updateLoaded(
    loaded: List[Option[SequenceView]],
    allIds: List[Observation.Id]
  ): SequencesOnDisplay = {
    // Build the new tabs
    val currentInsTabs = SequencesOnDisplay.instrumentTabs.getAll(this)
    val instTabs = loaded.collect {
      case Some(x) =>
        val tab = currentInsTabs
          .find(_.obsId === x.id)
        val curTableState = tab
          .map(_.tableState)
          .getOrElse(StepsTable.State.InitialTableState)
        val left         = tag[InstrumentSequenceTab.CompletedSV][SequenceView](x)
        val right        = tag[InstrumentSequenceTab.LoadedSV][SequenceView](x)
        val seq          = tab.filter(_.isComplete).as(left).toLeft(right)
        val stepConfig   = tab.flatMap(_.stepConfig)
        val selectedStep = tab.flatMap(_.selectedStep)
        val tabOperations = tab.map(_.tabOperations).getOrElse(TabOperations.Default)
        InstrumentSequenceTab(x.metadata.instrument,
                              seq,
                              stepConfig,
                              selectedStep,
                              curTableState,
                              tabOperations).some
    }

    // Store current focus
    val currentFocus = tabs.focus
    // Save the current preview
    val onlyPreview = SequencesOnDisplay.previewTab
      .headOption(this)
      .filter(p => allIds.contains(p.obsId))
    val sequenceTabs = (onlyPreview :: instTabs).collect { case Some(x) => x }
    // new zipper
    val newZipper =
      Zipper[SeqexecTab](Nil, CalibrationQueueTab.Empty, sequenceTabs)
    // Restore focus
    val q = newZipper.findFocus {
      case _: PreviewSequenceTab if currentFocus.isPreview =>
        true
      case _: PreviewSequenceTab =>
        false
      case c: CalibrationQueueTab =>
        currentFocus === c
      case i: InstrumentSequenceTab =>
        currentFocus match {
          case j: InstrumentSequenceTab => i.obsId === j.obsId
          case _                        => false
        }
    }
    copy(tabs = q.getOrElse(newZipper))
  }

  /**
    * Sets the sequence s as preview. if it is already loaded, it will focus there instead
    */
  def previewSequence(i: Instrument, s: SequenceView): SequencesOnDisplay = {
    val obsId    = s.id
    val isLoaded = loadedIds.contains(s.id)
    // Replace the sequence for the instrument or the completed sequence and reset displaying a step
    val seq = if (s.metadata.instrument === i && !isLoaded) {
      val newPreview = SequencesOnDisplay.previewTabById(obsId).isEmpty(this)
      val tsUpd = (ts: TableState[StepsTable.TableColumn]) =>
        if (newPreview) StepsTable.State.InitialTableState else ts
      val update =
        PreviewSequenceTab.tableState.modify(tsUpd) >>>
          PreviewSequenceTab.currentSequence.set(s) >>>
          PreviewSequenceTab.stepConfig.set(None)
      val q = withPreviewTab(s).tabs
        .findFocus(_.isPreview)
        .map(_.modify(SeqexecTab.previewTab.modify(update)))
      q
    } else if (isLoaded) {
      tabs.findFocusP {
        case InstrumentSequenceTab(_, Right(curr), _, _, _, _) =>
          obsId === curr.id
      }
    } else {
      tabs.some
    }
    copy(tabs = seq.getOrElse(tabs))
  }

  /**
    * Focus on the day calibration tab
    */
  def focusOnDayCal: SequencesOnDisplay = {
    val q = tabs.findFocus {
      case _: CalibrationQueueTab => true
      case _                      => false
    }
    copy(tabs = q.getOrElse(tabs))
  }

  /**
    * Adds a preview tab if empty
    */
  private def withPreviewTab(s: SequenceView): SequencesOnDisplay =
    // Note Traversal.isEmpty isn't valid here
    if (SequencesOnDisplay.previewTab.isEmpty(this)) {
      val ts = Zipper.fromNel(tabs.toNel.flatMap {
        case c: CalibrationQueueTab =>
          NonEmptyList.of(c,
                          PreviewSequenceTab(s,
                                             None,
                                             false,
                                             StepsTable.State.InitialTableState,
                                             TabOperations.Default))
        case t =>
          NonEmptyList.of(t)
      })
      SequencesOnDisplay.tabs.set(ts)(this)
    } else {
      this
    }

  /**
    * Focus on the preview tab
    */
  def focusOnPreview: SequencesOnDisplay = {
    val q = tabs.findFocus(_.isPreview)
    copy(tabs = q.getOrElse(tabs))
  }

  def unsetPreviewOn(id: Observation.Id): SequencesOnDisplay =
    if (SequencesOnDisplay.previewTab.exist(_.obsId === id)(this)) {
      // Store current focus
      val currentFocus = tabs.focus
      // Remove the sequence in the preview if it matches id
      copy(
        tabs = Zipper
          .fromNel(NonEmptyList.fromListUnsafe(tabs.toList.filter {
            case p: PreviewSequenceTab => p.obsId =!= id
            case _                     => true
          }))
          .findFocus {
            case _: PreviewSequenceTab if currentFocus.isPreview =>
              true
            case _: PreviewSequenceTab =>
              false
            case _: CalibrationQueueTab if currentFocus.isPreview =>
              true
            case c: CalibrationQueueTab =>
              currentFocus === c
            case i: InstrumentSequenceTab =>
              currentFocus match {
                case j: InstrumentSequenceTab => i.obsId === j.obsId
                case _                        => false
              }
          }
          .getOrElse(tabs))
    } else {
      this
    }

  // Is the id focused?
  def idDisplayed(id: Observation.Id): Boolean =
    tabs.withFocus.exists {
      case (InstrumentSequenceTab(_, Right(curr), _, _, _, _), true) =>
        curr.id === id
      case (PreviewSequenceTab(curr, _, _, _, _), true) =>
        curr.id === id
      case _ =>
        false
    }

  def tab(id: Observation.Id): Option[SeqexecTabActive] =
    tabs.withFocus.toList.collect {
      case (i: SequenceTab, a) if i.obsId === id =>
        val selected = if (a) TabSelected.Selected else TabSelected.Background
        SeqexecTabActive(i, selected)
    }.headOption

  def availableTabs
    : NonEmptyList[Either[CalibrationQueueTabActive, AvailableTab]] =
    NonEmptyList.fromListUnsafe(tabs.withFocus.toList.collect {
      case (i: InstrumentSequenceTab, a) =>
        AvailableTab(i.obsId,
                     i.sequence.status,
                     i.sequence.metadata.instrument,
                     i.runningStep,
                     i.nextStepToRun,
                     i.isPreview,
                     TabSelected.fromBoolean(a),
                     i.loading).asRight
      case (i: PreviewSequenceTab, a) =>
        AvailableTab(i.obsId,
                     i.sequence.status,
                     i.sequence.metadata.instrument,
                     i.runningStep,
                     i.nextStepToRun,
                     i.isPreview,
                     TabSelected.fromBoolean(a),
                     i.loading).asRight
      case (i: CalibrationQueueTab, a) =>
        CalibrationQueueTabActive(i, TabSelected.fromBoolean(a)).asLeft
    })

  def cleanAll: SequencesOnDisplay =
    SequencesOnDisplay.Empty

  /**
    * Operator of the instrument tab if in focus
    */
  def selectedObserver
    : Option[Either[DayCalObserverFocus, SequenceObserverFocus]] =
    SequencesOnDisplay.focusSequence
      .getOption(this)
      .collect {
        case InstrumentSequenceTab(_, Right(s), _, _, _, _) =>
          SequenceObserverFocus(s.metadata.instrument,
                                s.id,
                                s.allStepsDone,
                                s.metadata.observer).asRight
      }
      .orElse {
        SequencesOnDisplay.focusQueue.getOption(this).collect {
          case CalibrationQueueTab(_, _, o) =>
            DayCalObserverFocus(CalibrationQueueId, o).asLeft
        }
      }

  // Update the state when load has completed
  def loadingComplete(
    obsId: Observation.Id,
    i:     Instrument
  ): SequencesOnDisplay = {
    // This is a bit tricky. When we load we need to remove existing completed sequences
    // As this is a client side only state it won't be cleaned automatically
    val cleaned = copy(
      tabs = Zipper.fromNel(NonEmptyList.fromListUnsafe(tabs.toList.filter {
        case InstrumentSequenceTab(inst, Left(_), _, _, _, _) => inst =!= i
        case _                                                => true
      })))

    SequencesOnDisplay.loadingL(obsId).set(false)(cleaned)
  }

  // Reset the loading state client-side
  def loadingFailed(obsId: Observation.Id): SequencesOnDisplay =
    SequencesOnDisplay.loadingL(obsId).set(false)(this)

  // We'll set the passed SequenceView as completed for the given instruments
  def markCompleted(completed: SequenceView): SequencesOnDisplay =
    (SequencesOnDisplay.instrumentTabFor(completed.metadata.instrument) ^|-> InstrumentSequenceTab.curSequence)
      .set(
        tag[InstrumentSequenceTab.CompletedSV][SequenceView](completed).asLeft)(
        this)

  // Update the state when a load starts
  def markAsLoading(id: Observation.Id): SequencesOnDisplay =
    SequencesOnDisplay.loadingL(id).set(true)(this)

  def stepsTables: Map[Observation.Id, TableState[StepsTable.TableColumn]] =
    SequencesOnDisplay.sequenceTabs
      .getAll(this)
      .collect {
        case i: InstrumentSequenceTab =>
          (i.sequence.id, i.tableState)
        case PreviewSequenceTab(curr, _, _, tableState, _) =>
          (curr.id, tableState)
      }
      .toMap

  def updateTableStates(
    stepsTables: Map[Observation.Id, TableState[StepsTable.TableColumn]]
  ): SequencesOnDisplay =
    copy(tabs = tabs.map {
      case i: InstrumentSequenceTab =>
        stepsTables
          .get(i.obsId)
          .map(s => i.copy(tableState = s))
          .getOrElse(i)
      case i @ PreviewSequenceTab(curr, _, _, _, _) =>
        stepsTables
          .get(curr.id)
          .map(s => i.copy(tableState = s))
          .getOrElse(i)
      case i => i
    })

  def markOperations(
    id:      Observation.Id,
    updater: TabOperations => TabOperations
  ): SequencesOnDisplay =
    (SequencesOnDisplay.instrumentTabById(id) ^|-> InstrumentSequenceTab.tabOperations)
      .modify(updater)(this)

  def resetOperations(id: Observation.Id): SequencesOnDisplay =
    markOperations(id, _ => TabOperations.Default)

  def resetAllOperations: SequencesOnDisplay =
    loadedIds.foldLeft(this)((sod, id) => sod.resetOperations(id))

  def resetAllResourceOperations(id: Observation.Id): SequencesOnDisplay =
    markOperations(id, TabOperations.clearResourceOperations)

  def selectStep(
    id:   Observation.Id,
    step: StepId
  ): SequencesOnDisplay =
    (SequencesOnDisplay.instrumentTabById(id) ^|-> InstrumentSequenceTab.selected)
      .set(step.some)(this)

}

/**
  * Contains the sequences displayed on the instrument tabs. Note that they are references to sequences on the Queue
  */
@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SequencesOnDisplay {
  // We need to initialize the model with something so we use preview
  val Empty: SequencesOnDisplay =
    SequencesOnDisplay(
      Zipper.fromNel[SeqexecTab](NonEmptyList.of(CalibrationQueueTab.Empty)))

  implicit val eq: Eq[SequencesOnDisplay] =
    Eq.by(_.tabs)

  private def previewMatch(id: Observation.Id)(tab: SeqexecTab): Boolean =
    tab match {
      case PreviewSequenceTab(curr, _, _, _, _) => curr.id === id
      case _                                    => false
    }

  def previewTabById(
    id: Observation.Id
  ): Traversal[SequencesOnDisplay, PreviewSequenceTab] =
    SequencesOnDisplay.tabs ^|->> Zipper.unsafeFilterZ(previewMatch(id)) ^<-? SeqexecTab.previewTab

  private def sequenceMatch(id: Observation.Id)(tab: SeqexecTab): Boolean =
    tab match {
      case InstrumentSequenceTab(_, Right(curr), _, _, _, _) => curr.id === id
      case _                                                 => false
    }

  def instrumentTabById(
    id: Observation.Id
  ): Traversal[SequencesOnDisplay, InstrumentSequenceTab] =
    SequencesOnDisplay.tabs ^|->> Zipper.unsafeFilterZ(sequenceMatch(id)) ^<-? SeqexecTab.instrumentTab

  val previewTab: Traversal[SequencesOnDisplay, PreviewSequenceTab] =
    SequencesOnDisplay.tabs ^|->> Zipper.unsafeFilterZ(_.isPreview) ^<-? SeqexecTab.previewTab

  private def instrumentMatch(i: Instrument)(tab: SeqexecTab): Boolean =
    tab match {
      case t: InstrumentSequenceTab =>
        t.inst === i
      case _ => false
    }

  def instrumentTabFor(
    i: Instrument
  ): Traversal[SequencesOnDisplay, InstrumentSequenceTab] =
    SequencesOnDisplay.tabs                    ^|->>
      Zipper.unsafeFilterZ(instrumentMatch(i)) ^<-?
      SeqexecTab.instrumentTab

  private def instrumentTab(tab: SeqexecTab): Boolean =
    tab match {
      case _: InstrumentSequenceTab => true
      case _                        => false
    }

  val instrumentTabs: Traversal[SequencesOnDisplay, InstrumentSequenceTab] =
    SequencesOnDisplay.tabs               ^|->>
      Zipper.unsafeFilterZ(instrumentTab) ^<-?
      SeqexecTab.instrumentTab

  private def sequenceTab(tab: SeqexecTab): Boolean =
    tab match {
      case _: InstrumentSequenceTab => true
      case _: PreviewSequenceTab    => true
      case _                        => false
    }

  val sequenceTabs: Traversal[SequencesOnDisplay, SequenceTab] =
    SequencesOnDisplay.tabs             ^|->>
      Zipper.unsafeFilterZ(sequenceTab) ^<-?
      SeqexecTab.sequenceTab

  private def completedTab(tab: SeqexecTab): Boolean =
    tab match {
      case t: InstrumentSequenceTab => t.isComplete
      case _                        => false
    }

  val completedTabs: Traversal[SequencesOnDisplay, InstrumentSequenceTab.CompletedSequenceView] =
    SequencesOnDisplay.tabs                   ^|->>
      Zipper.unsafeFilterZ(completedTab)      ^<-?
      SeqexecTab.instrumentTab                ^|-?
      InstrumentSequenceTab.completedSequence

  // Optional to the selected tab if on focus
  val focusSequence: Optional[SequencesOnDisplay, SequenceTab] =
    SequencesOnDisplay.tabs ^|-> Zipper.focus ^<-? SeqexecTab.sequenceTab

  // Optional to the calibration tab if on focus
  val focusQueue: Optional[SequencesOnDisplay, CalibrationQueueTab] =
    SequencesOnDisplay.tabs ^|-> Zipper.focus ^<-? SeqexecTab.calibrationTab

  val calTabObserver: Optional[SequencesOnDisplay, Observer] =
    focusQueue                     ^|->
      CalibrationQueueTab.observer ^<-?
      std.option.some

  val availableTabsG
    : Getter[SequencesOnDisplay,
             NonEmptyList[Either[CalibrationQueueTabActive, AvailableTab]]] =
    Getter(_.availableTabs)

  val stepL: Optional[SeqexecTab, Option[Int]] =
    SeqexecTab.sequenceTab ^|-> SequenceTab.stepConfigL

  def loadingL(id: Observation.Id): Traversal[SequencesOnDisplay, Boolean] =
    SequencesOnDisplay.previewTabById(id) ^|-> PreviewSequenceTab.isLoading

  def tabG(
    id: Observation.Id
  ): Getter[SequencesOnDisplay, Option[SeqexecTabActive]] =
    SequencesOnDisplay.tabs.asGetter >>> {
      _.withFocus.toList.collect {
        case (i: SequenceTab, a) if i.obsId === id =>
          val selected =
            if (a) TabSelected.Selected else TabSelected.Background
          SeqexecTabActive(i, selected)
      }.headOption
    }
}
