// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

object operations {
  // Operations possible at the sequence level
  sealed trait SequenceOperations

  object SequenceOperations {
    case object PauseSequence extends SequenceOperations
    case object ContinueSequence extends SequenceOperations
    case object StopSequence extends SequenceOperations
    case object RunSequence extends SequenceOperations
  }

  // Operations possible at the observation level
  sealed trait ObservationOperations

  object ObservationOperations {
    case object PauseObservation extends ObservationOperations
    case object StopObservation extends ObservationOperations
    case object AbortObservation extends ObservationOperations
    case object ResumeObservation extends ObservationOperations
    // Operations for Hamamatsu
    case object PauseImmediatelyObservation extends ObservationOperations
    case object StopImmediatelyObservation extends ObservationOperations
    case object PauseGracefullyObservation extends ObservationOperations
    case object StopGracefullyObservation extends ObservationOperations
  }

}
