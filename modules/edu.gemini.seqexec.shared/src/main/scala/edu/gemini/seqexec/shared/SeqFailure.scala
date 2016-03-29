package edu.gemini.seqexec.shared

import edu.gemini.pot.sp.SPObservationID

sealed trait SeqFailure

object SeqFailure {
  case class MissingObservation(oid: SPObservationID) extends SeqFailure
  case class SeqException(ex: Throwable) extends SeqFailure

  def explain(sf: SeqFailure): String = sf match {
    case MissingObservation(oid) =>
      s"The database doesn't have observation $oid"

    case SeqException(ex)        =>
      s"Exception: ${ex.getMessage}"
  }
}

