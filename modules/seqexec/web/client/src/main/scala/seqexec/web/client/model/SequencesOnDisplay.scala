// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import gem.Observation
import seqexec.model.{ SequencesQueue, SequenceView }
import seqexec.model.enum._
import seqexec.web.common.Zipper
import seqexec.web.client.circuit.SequenceObserverFocus
import seqexec.web.client.ModelOps._

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
    sequences.toNel.collect {
      case InstrumentSequenceTab(_, Some(curr), _, _) => curr.id
    }

  /**
   * List of sequence ids on tabs
   */
  val tabIds: List[Observation.Id] =
    sequences.toNel.collect {
      case InstrumentSequenceTab(_, Some(curr), _, _) => curr.id
      case PreviewSequenceTab(Some(curr), _) => curr.id
    }

  def updateFromQueue(s: SequencesQueue[SequenceView]): SequencesOnDisplay = {
    val updated = updateLoaded(s.loaded.values.toList.map { id =>
      s.queue.find(_.id === id)
    }).sequences.map {
      case p @ PreviewSequenceTab(Some(curr), r) => s.queue.find(_.id === curr.id).map(s => PreviewSequenceTab(Some(s), r)).getOrElse(p)
      case t => t
    }
    SequencesOnDisplay(updated)
  }
  /**
   * Replace the list of loaded sequences
   */
  private def updateLoaded(s: List[Option[SequenceView]]): SequencesOnDisplay = {
    // Build the new tabs
    val instTabs = s.collect { case Some(x) => InstrumentSequenceTab(x.metadata.instrument, x.some, None, None)  }
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
      case PreviewSequenceTab(_, _) =>
        false
      case InstrumentSequenceTab(i, _, _, _)                  =>
        currentFocus match {
          case InstrumentSequenceTab(j, _, _, _) => i === j
          case PreviewSequenceTab(_, _)          => false
        }
    }
    copy(sequences = q.getOrElse(newZipper))
  }

  /**
   * Sets the passed sequences as preview. if it is already loaded, it will focus there instead
   */
  def previewSequence(i: Instrument, s: Option[SequenceView]): SequencesOnDisplay = {
    val obsId = s.map(_.id)
    val isLoaded = obsId.exists(loadedIds.contains)
    // Replace the sequence for the instrument or the completed sequence and reset displaying a step
    val seq = if (s.exists(x => x.metadata.instrument === i && !isLoaded)) {
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

  def unsetPreviewOn(id: Observation.Id): SequencesOnDisplay = {
    // Remove the sequence in the preview if it matches id
    val q = sequences.map {
      case s @ PreviewSequenceTab(cur, _) if cur.exists(_.id === id) =>
        SequenceTab.currentSequenceL.set(None)(s)
      case s                                                      =>
        s
    }
    copy(sequences = q)
  }

  def unsetPreview: SequencesOnDisplay = {
    // Remove any sequence in the preview
    val q = sequences.map {
      case s if s.isPreview => SequenceTab.currentSequenceL.set(None)(s)
      case s                => s
    }
    copy(sequences = q)
  }

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
    }.map(_.modify(SequenceTab.completedSequenceO.set(completed.some)(_)))

    copy(sequences = q.getOrElse(sequences))
  }

  def availableTabs: NonEmptyList[AvailableTab] =
    NonEmptyList.fromListUnsafe(sequences.withFocus.map {
      case (i, a) => AvailableTab(i.sequence.map(_.id), i.sequence.map(_.status), i.instrument, i.runningStep, i.isPreview, a)
    }.toList)

  def cleanAll: SequencesOnDisplay =
    SequencesOnDisplay.empty

  def selectedOperator: Option[SequenceObserverFocus] = {
    val f = sequences.focus
    f.sequence.map { s => SequenceObserverFocus(s.metadata.instrument, s.id, s.allStepsDone, s.metadata.observer) }.filter(_ => !f.isPreview)
  }
}

/**
  * Contains the sequences displayed on the instrument tabs. Note that they are references to sequences on the Queue
  */
object SequencesOnDisplay {
  // We need to initialize the model with something so we use preview
  val empty: SequencesOnDisplay = SequencesOnDisplay(Zipper.fromNel(NonEmptyList.of(SequenceTab.Empty)))

  implicit val eq: Eq[SequencesOnDisplay] =
    Eq.by(_.sequences)
}
