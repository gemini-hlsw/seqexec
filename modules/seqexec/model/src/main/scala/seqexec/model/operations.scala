// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import seqexec.model.Model.{Instrument, Step}
import seqexec.model.Model.Instrument._
import mouse.boolean._

object operations {
  // Operations possible at the sequence level
  sealed trait SequenceOperations

  object SequenceOperations {
    case object PauseSequence extends SequenceOperations
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

  sealed trait SupportedOperations {
    /**
     * Sorted list of operations supported at the sequence level
     */
    def observationOperations(s: Step): List[ObservationOperations]

    /**
     * Sorted list of operations supported at the observation (row) level
     */
    def sequenceOperations: List[SequenceOperations]
  }

  private val F2SupportedOperations = new SupportedOperations {
    def observationOperations(s: Step): List[ObservationOperations] = Nil
    def sequenceOperations: List[SequenceOperations] = Nil
  }

  private val GmosSupportedOperations = new SupportedOperations {
    def observationOperations(s: Step): List[ObservationOperations] =
      s.isObservePaused.fold(List(ObservationOperations.ResumeObservation, ObservationOperations.StopObservation, ObservationOperations.AbortObservation), List(ObservationOperations.PauseObservation, ObservationOperations.StopObservation, ObservationOperations.AbortObservation))

    def sequenceOperations: List[SequenceOperations] = Nil
  }

  private val GnirsSupportedOperations = new SupportedOperations {
    def observationOperations(s: Step): List[ObservationOperations] =
      s.isObservePaused.fold(List(ObservationOperations.StopObservation, ObservationOperations.AbortObservation), List(ObservationOperations.StopObservation, ObservationOperations.AbortObservation))

    def sequenceOperations: List[SequenceOperations] = Nil
  }

  private val NilSupportedOperations = new SupportedOperations {
    def observationOperations(s: Step): List[ObservationOperations] = Nil
    def sequenceOperations: List[SequenceOperations] = Nil
  }

  private val instrumentOperations: Map[Instrument, SupportedOperations] = Map(
    (F2    -> F2SupportedOperations),
    (GmosS -> GmosSupportedOperations),
    (GmosN -> GmosSupportedOperations),
    (GNIRS -> GnirsSupportedOperations)
  )

  final implicit class SupportedOperationsOps(val i: Instrument) extends AnyVal {
    def observationOperations(s: Step): List[ObservationOperations] =
      instrumentOperations.getOrElse(i, NilSupportedOperations).observationOperations(s)
    def sequenceOperations: List[SequenceOperations] =
      instrumentOperations.getOrElse(i, NilSupportedOperations).sequenceOperations
  }

}
