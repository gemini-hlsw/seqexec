package edu.gemini.seqexec.web.client.model

import diode.data.{Empty, Pot, PotAction}
import edu.gemini.seqexec.web.common.{SeqexecQueue, Sequence}

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

// Action to add a sequence to the queue
case class AddToQueue(s: Sequence)
// Action to remove a sequence from the search results
case class RemoveFromSearch(s: Sequence)

// End Actions

// UI model
sealed trait SearchAreaState
case object SearchAreaOpen extends SearchAreaState
case object SearchAreaClosed extends SearchAreaState

case class SequenceTab(instrument: SeqexecAppRootModel.Instrument, sequence: Option[Sequence])

// Model for the tabbed area of sequences
case class SequencesOnDisplay(instrumentSequences: List[SequenceTab], freeSequences: List[SequenceTab], focus: SequenceTab) {
}

object SequencesOnDisplay {
  val empty = SequencesOnDisplay(SeqexecAppRootModel.instruments.map(SequenceTab(_, None)), Nil, SeqexecAppRootModel.instruments.map(SequenceTab(_, None)).head)
}

/**
  * Root of the UI Model of the application
  */
case class SeqexecAppRootModel(queue: Pot[SeqexecQueue], searchAreaState: SearchAreaState, searchResults: Pot[List[Sequence]], sequencesOnDisplay: SequencesOnDisplay)

object SeqexecAppRootModel {
  // Placeholder for the instrument type
  type Instrument = String

  // TODO Replace these for  a real list of instruments
  // TODO This list should be site-specific
  val instruments = List[Instrument]("F2", "GMOS-S", "GPI", "GSAOI")
}