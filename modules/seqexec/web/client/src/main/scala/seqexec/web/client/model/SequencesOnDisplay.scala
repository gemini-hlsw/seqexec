// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.implicits._
import cats.data.NonEmptyList
import diode.RootModelR
import diode.data._
import gem.Observation
import seqexec.model.SequenceView
import seqexec.model.enum._
import seqexec.web.common.Zipper

// Model for the tabbed area of sequences
final case class SequencesOnDisplay(sequences: Zipper[SequenceTab]) {
  // Display a given step on the focused sequence
  def showStepConfig(id: Observation.Id, i: Int): SequencesOnDisplay =
    if (sequences.focus.obsId.exists(_ === id)) {
      copy(sequences = sequences.modify(SequenceTab.stepConfigL.set(Some(i))(_)))
    } else {
      this
    }

  // Don't show steps for the sequence
  def hideStepConfig: SequencesOnDisplay =
    copy(sequences = sequences.modify(SequenceTab.stepConfigL.set(None)(_)))

  def focusOnSequence(inst: Instrument, id: Observation.Id): SequencesOnDisplay = {
    // Replace the sequence for the instrument or the completed sequence and reset displaying a step
    val q = sequences.findFocus(i => i.sequence.exists(s => s.id === id && s.metadata.instrument === inst))
    copy(sequences = q.getOrElse(sequences))
  }

  /**
   * List of loaded sequence ids
   */
  val loadedIds: List[Observation.Id] =
    sequences.toList.collect {
      case InstrumentSequenceTab(_, curr, _, _) => curr().map(_.id)
    }.collect {
      case Some(x) => x
    }

  /**
   * Replace the list of loaded sequences
   */
  def updateLoaded(s: List[RefTo[Option[SequenceView]]]): SequencesOnDisplay = {
    // Build the new tabs
    val instTabs = s.fproduct(_()).collect { case (r, Some(x)) => InstrumentSequenceTab(x.metadata.instrument, r, None, None)  }
    // Store current focus
    val currentFocus = sequences.focus
    // Save the current preview
    val onlyPreview = sequences.toList.filter{
      case x => x.isPreview
    }.headOption.getOrElse(SequenceTab.Empty)
    // new zipper
    val newZipper = Zipper[SequenceTab](Nil, onlyPreview, instTabs)
    // Restore focus
    val q = newZipper.findFocus {
      case PreviewSequenceTab(_, _) if currentFocus.isPreview =>
        true
      case InstrumentSequenceTab(i, _, _, _)                  =>
        currentFocus match {
          case InstrumentSequenceTab(j, _, _, _) => i === j
          case PreviewSequenceTab(_, _)          => false
        }
    }
    copy(sequences = q.getOrElse(sequences))
  }

  /**
   * Sets the passed sequences as preview. if it is already loaded, it will focus there instead
   */
  def previewSequence(i: Instrument, s: RefTo[Option[SequenceView]]): SequencesOnDisplay = {
    val obsId = s().map(_.id)
    val isLoaded = obsId.exists(loadedIds.contains)
    // Replace the sequence for the instrument or the completed sequence and reset displaying a step
    val seq = if (s().exists(x => x.metadata.instrument === i && !isLoaded)) {
      val q = sequences.findFocus(_.isPreview).map(_.modify((SequenceTab.currentSequenceL.set(s) andThen SequenceTab.stepConfigL.set(None))(_)))
      q.getOrElse(sequences)
    } else if (isLoaded) {
      sequences.findFocus(t => !t.isPreview && obsId === t.obsId).getOrElse(sequences)
    } else {
      sequences
    }
    copy(sequences = seq)
  }

  /**
   * Focus on the preview tab
   */
  def focusOnPreview: SequencesOnDisplay = {
    val q = sequences.findFocus(_.isPreview)
    copy(sequences = q.getOrElse(sequences))
  }

  def unsetPreviewOn(i: Observation.Id): SequencesOnDisplay = {
    // Remove any sequence in the preview
    val q = sequences.map {
      case s @ PreviewSequenceTab(cur, _) if cur().exists(_.id === i) =>
        SequenceTab.currentSequenceL.set(SequencesOnDisplay.emptySeqRef)(s)
      case s                                                      =>
        s
    }
    copy(sequences = q)
  }

  def unsetPreview: SequencesOnDisplay = {
    // Remove any sequence in the preview
    val q = sequences.map {
      case s if s.isPreview => SequenceTab.currentSequenceL.set(SequencesOnDisplay.emptySeqRef)(s)
      case s                => s
    }
    copy(sequences = q)
  }

  def isAnySelected: Boolean = sequences.exists(_.sequence.isDefined)

  // Is the id on the sequences area?
  def idDisplayed(id: Observation.Id): Boolean =
    sequences.withFocus.exists { case (s, a) => a && s.sequence.exists(_.id === id) }

  def tab(id: Observation.Id): Option[SequenceTabActive] =
    sequences.withFocus.find(_._1.obsId.exists(_ === id))
      .map { case (i, a) => SequenceTabActive(i, a) }

  // We'll set the passed SequenceView as completed for the given instruments
  def markCompleted(completed: SequenceView): SequencesOnDisplay = {
    val q = sequences.findFocus {
      case t: InstrumentSequenceTab => t.instrument === completed.metadata.instrument.some
      case _                        => false
    }.map(_.modify(SequenceTab.completedSequenceL.set(completed.some)(_)))

    copy(sequences = q.getOrElse(sequences))
  }

  def availableTabs: NonEmptyList[AvailableTab] =
    NonEmptyList.fromListUnsafe(sequences.withFocus.map {
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
