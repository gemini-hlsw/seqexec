// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import gem.Observation
import monocle.{ Optional, Traversal }
import monocle.macros.Lenses
import seqexec.model.{ SequencesQueue, SequenceView }
import seqexec.model.enum._
import seqexec.web.common.Zipper
import seqexec.web.client.circuit.SequenceObserverFocus
import seqexec.web.client.components.sequence.steps.StepsTable
import seqexec.web.client.ModelOps._
import web.client.table._

// Model for the tabbed area of sequences
@Lenses
final case class SequencesOnDisplay(tabs: Zipper[SeqexecTab]) {
  // Display a given step on the focused sequence
  def showStepConfig(id: Observation.Id, i: Int): SequencesOnDisplay = {
    if (SequencesOnDisplay.focusSequence.getOption(this).exists(_.obsId.exists(_ === id))) {
      SequencesOnDisplay.tabs.modify(_.modify((SeqexecTab.sequenceTab ^|-> SequenceTab.stepConfigL).set(Some(i))))(this)
    } else {
      this
    }
  }

  // Don't show steps for the sequence
  def hideStepConfig: SequencesOnDisplay =
    SequencesOnDisplay.tabs.modify(_.modify((SeqexecTab.sequenceTab ^|-> SequenceTab.stepConfigL).set(None)))(this)

  // Focus on a tab for the instrument and id
  def focusOnSequence(inst: Instrument, id: Observation.Id): SequencesOnDisplay = {
    val q = tabs.findFocusP {
      case t: SequenceTab => t.sequence.exists(s => s.id === id && s.metadata.instrument === inst)
    }
    copy(tabs = q.getOrElse(tabs))
  }

  /**
   * List of loaded sequence ids
   */
  def loadedIds: List[Observation.Id] =
    tabs.toNel.collect {
      case InstrumentSequenceTab(_, Some(curr), _, _, _, _) => curr.id
    }

  /**
   * Replace the tabs when the core model is updated
   */
  def updateFromQueue(s: SequencesQueue[SequenceView]): SequencesOnDisplay = {
    val updated = updateLoaded(s.loaded.values.toList.map { id =>
      s.queue.find(_.id === id)
    }).tabs.map {
      case p @ PreviewSequenceTab(Some(curr), r, _, t, o) =>
        s.queue.find(_.id === curr.id)
          .map(s => PreviewSequenceTab(Some(s), r, false, t, o))
          .getOrElse(p)
      case t => t
    }
    SequencesOnDisplay(updated)
  }

  /**
   * Replace the list of loaded sequences
   */
  private def updateLoaded(s: List[Option[SequenceView]]): SequencesOnDisplay = {
    // Build the new tabs
    val currentInsTabs = SequencesOnDisplay.instrumentTabs.getAll(this)
    val instTabs = s.collect { case Some(x) =>
      val curTableState = currentInsTabs.find(_.obsId.exists(_ === x.id)).map(_.tableState).getOrElse(StepsTable.State.InitialTableState)
      InstrumentSequenceTab(x.metadata.instrument, x.some, None, None, curTableState, TabOperations.Default)
    }
    // Store current focus
    val currentFocus = tabs.focus
    // Save the current preview
    val onlyPreview = SequencesOnDisplay.previewTab.headOption(this)
    // new zipper
    val newZipper = Zipper[SeqexecTab](Nil, onlyPreview.getOrElse(SequenceTab.Empty), instTabs)
    // Restore focus
    val q = newZipper.findFocus {
      case _: PreviewSequenceTab if currentFocus.isPreview =>
        true
      case _: PreviewSequenceTab                           =>
        false
      case _: CalibrationQueueTab                          =>
        false
      case InstrumentSequenceTab(i, _, _, _, _, _)         =>
        currentFocus match {
          case InstrumentSequenceTab(j, _, _, _, _, _) => i === j
          case _: PreviewSequenceTab                   => false
          case _: CalibrationQueueTab                  => false
        }
    }
    copy(tabs = q.getOrElse(newZipper))
  }

  /**
   * Sets the sequence s as preview. if it is already loaded, it will focus there instead
   */
  def previewSequence(i: Instrument, s: Option[SequenceView]): SequencesOnDisplay = {
    val obsId = s.map(_.id)
    val isLoaded = obsId.exists(loadedIds.contains)
    // Replace the sequence for the instrument or the completed sequence and reset displaying a step
    val seq = if (s.exists(x => x.metadata.instrument === i && !isLoaded)) {
      val q = tabs.findFocus(_.isPreview)
        .map(_.modify(SeqexecTab.previewTab.modify((PreviewSequenceTab.currentSequence.set(s) andThen PreviewSequenceTab.stepConfig.set(None))(_))))
      q
    } else if (isLoaded) {
      tabs.findFocusP {
        case InstrumentSequenceTab(_, Some(curr), _, _, _, _) => obsId.exists(_ === curr.id)
      }
    } else {
      tabs.some
    }
    copy(tabs = seq.getOrElse(tabs))
  }

  /**
   * Focus on the preview tab
   */
  def focusOnPreview: SequencesOnDisplay = {
    val q = tabs.findFocus(_.isPreview)
    copy(tabs = q.getOrElse(tabs))
  }

  def unsetPreviewOn(id: Observation.Id): SequencesOnDisplay =
    // Remove the sequence in the preview if it matches id
    (SequencesOnDisplay.previewTabById(id) ^|-> PreviewSequenceTab.currentSequence).set(None)(this)

  def unsetPreview: SequencesOnDisplay =
    // Remove any sequence in the preview
    (SequencesOnDisplay.previewTab ^|-> PreviewSequenceTab.currentSequence).set(None)(this)

  // Is the id focused?
  def idDisplayed(id: Observation.Id): Boolean =
    tabs.withFocus.exists {
      case (InstrumentSequenceTab(_, Some(curr), _, _, _, _), true) => curr.id === id
      case (PreviewSequenceTab(Some(curr), _, _, _, _), true)       => curr.id === id
      case _                                                        => false
    }

  def tab(id: Observation.Id): Option[SeqexecTabActive] =
    tabs.withFocus.toList.collect {
      case (i: SequenceTab, a) if i.obsId.exists(_ === id) =>
        val selected = if (a) TabSelected.Selected else TabSelected.Background
        SeqexecTabActive(i, selected)
    }.headOption

  def availableTabs: NonEmptyList[AvailableTab] =
    NonEmptyList.fromListUnsafe(tabs.withFocus.toList.collect {
      case (i: InstrumentSequenceTab, a) => AvailableTab(i.sequence.map(_.id), i.sequence.map(_.status), i.instrument, i.runningStep, i.nextStepToRun, i.isPreview, a, i.loading)
      case (i: PreviewSequenceTab, a) => AvailableTab(i.sequence.map(_.id), i.sequence.map(_.status), i.instrument, i.runningStep, i.nextStepToRun, i.isPreview, a, i.loading)
    })

  def cleanAll: SequencesOnDisplay =
    SequencesOnDisplay.Empty

  /**
   * Operator of the instrument tab if in focus
   */
  def selectedOperator: Option[SequenceObserverFocus] =
    SequencesOnDisplay.focusSequence.getOption(this).collect {
      case InstrumentSequenceTab(_, Some(s), _, _, _, _) =>
        SequenceObserverFocus(s.metadata.instrument, s.id, s.allStepsDone, s.metadata.observer)
    }

  // Update the state when a load has failed
  def loadingComplete(id: Observation.Id): SequencesOnDisplay =
    (SequencesOnDisplay.previewTabById(id) ^|-> PreviewSequenceTab.isLoading).set(false)(this)

  // We'll set the passed SequenceView as completed for the given instruments
  def markCompleted(completed: SequenceView): SequencesOnDisplay =
    (SequencesOnDisplay.instrumentTabFor(completed) ^|-> InstrumentSequenceTab.completedSequence).set(completed.some)(this)

  // Update the state when a load starts
  def markAsLoading(id: Observation.Id): SequencesOnDisplay =
    (SequencesOnDisplay.previewTabById(id) ^|-> PreviewSequenceTab.isLoading).set(true)(this)


  def stepsTables: Map[Observation.Id, TableState[StepsTable.TableColumn]] =
    SequencesOnDisplay.sequenceTabs.getAll(this).collect {
      case InstrumentSequenceTab(_, Some(curr), _, _, tableState, _) => (curr.id, tableState)
      case PreviewSequenceTab(Some(curr), _, _, tableState, _)       => (curr.id, tableState)
    }.toMap

  def updateStepsTableStates(stepsTables: Map[Observation.Id, TableState[StepsTable.TableColumn]]): SequencesOnDisplay =
    copy(tabs = tabs.map {
      case i @ InstrumentSequenceTab(_, Some(curr), _, _, _, _) =>
        stepsTables.get(curr.id)
          .map(s => i.copy(tableState = s))
          .getOrElse(i)
      case i @ PreviewSequenceTab(Some(curr), _, _, _, _) =>
        stepsTables.get(curr.id)
          .map(s => i.copy(tableState = s))
          .getOrElse(i)
      case i => i
    })

  def markOperations(id: Observation.Id, updater: TabOperations => TabOperations): SequencesOnDisplay =
    (SequencesOnDisplay.instrumentTabById(id) ^|-> InstrumentSequenceTab.tabOperations).modify(updater)(this)

}

/**
  * Contains the sequences displayed on the instrument tabs. Note that they are references to sequences on the Queue
  */
@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SequencesOnDisplay {
  // We need to initialize the model with something so we use preview
  val Empty: SequencesOnDisplay = SequencesOnDisplay(Zipper.fromNel[SeqexecTab](NonEmptyList.of(SequenceTab.Empty)))

  implicit val eq: Eq[SequencesOnDisplay] =
    Eq.by(_.tabs)

  private def previewMatch(id: Observation.Id)(tab: SeqexecTab): Boolean =
    tab match {
      case PreviewSequenceTab(Some(curr), _, _, _, _) => curr.id === id
      case _                                       => false
    }

  def previewTabById(id: Observation.Id): Traversal[SequencesOnDisplay, PreviewSequenceTab] =
    SequencesOnDisplay.tabs ^|->> Zipper.unsafeFilterZ(previewMatch(id)) ^<-? SeqexecTab.previewTab

  private def sequenceMatch(id: Observation.Id)(tab: SeqexecTab): Boolean =
    tab match {
      case InstrumentSequenceTab(_, Some(curr), _, _, _, _) => curr.id === id
      case _                                                => false
    }

  def instrumentTabById(id: Observation.Id): Traversal[SequencesOnDisplay, InstrumentSequenceTab] =
    SequencesOnDisplay.tabs ^|->> Zipper.unsafeFilterZ(sequenceMatch(id)) ^<-? SeqexecTab.instrumentTab

  val previewTab: Traversal[SequencesOnDisplay, PreviewSequenceTab] =
    SequencesOnDisplay.tabs ^|->> Zipper.unsafeFilterZ(_.isPreview) ^<-? SeqexecTab.previewTab

  private def instrumentMatch(seq: SequenceView)(tab: SeqexecTab): Boolean =
    tab match {
      case InstrumentSequenceTab(inst, _, _, _, _, _) => inst === seq.metadata.instrument
      case _                                            => false
    }

  def instrumentTabFor(seq: SequenceView): Traversal[SequencesOnDisplay, InstrumentSequenceTab] =
    SequencesOnDisplay.tabs ^|->> Zipper.unsafeFilterZ(instrumentMatch(seq)) ^<-? SeqexecTab.instrumentTab

  private def instrumentTab(tab: SeqexecTab): Boolean =
    tab match {
      case i: InstrumentSequenceTab => i.currentSequence.isDefined
      case _                        => false
    }

  val instrumentTabs: Traversal[SequencesOnDisplay, InstrumentSequenceTab] =
    SequencesOnDisplay.tabs ^|->> Zipper.unsafeFilterZ(instrumentTab) ^<-? SeqexecTab.instrumentTab

  private def sequenceTab(tab: SeqexecTab): Boolean =
    tab match {
      case _: InstrumentSequenceTab => true
      case _: PreviewSequenceTab    => true
      case _                        => false
    }

  val sequenceTabs: Traversal[SequencesOnDisplay, SequenceTab] =
    SequencesOnDisplay.tabs ^|->> Zipper.unsafeFilterZ(sequenceTab) ^<-? SeqexecTab.sequenceTab

  val focusSequence: Optional[SequencesOnDisplay, SequenceTab] =
    SequencesOnDisplay.tabs ^|-> Zipper.focus ^<-? SeqexecTab.sequenceTab
}
