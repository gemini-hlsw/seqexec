// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import edu.gemini.seqexec.model.Model.Instrument
import edu.gemini.seqexec.model.Model.Instrument._

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

  sealed trait SupportedOperations {
    def observationOperations: List[ObservationOperations]
    def sequenceOperations: List[SequenceOperations]
  }

  private val F2SupportedOperations = new SupportedOperations {
    def observationOperations: List[ObservationOperations] = Nil
    def sequenceOperations: List[SequenceOperations] = Nil
  }

  private val GmosSupportedOperations = new SupportedOperations {
    def observationOperations: List[ObservationOperations] = List(ObservationOperations.StopObservation, ObservationOperations.AbortObservation)
    def sequenceOperations: List[SequenceOperations] = Nil
  }

  private val NilSupportedOperations = new SupportedOperations {
    def observationOperations: List[ObservationOperations] = Nil
    def sequenceOperations: List[SequenceOperations] = Nil
  }

  def instrumentOperations(a: Instrument): SupportedOperations = a match {
    case F2    => F2SupportedOperations
    case GmosS => GmosSupportedOperations
    case GmosN => GmosSupportedOperations
    case _     => NilSupportedOperations
  }

  final class SupportedOperationsOps(val self: SupportedOperations) extends AnyVal {
    def observationOperations: List[ObservationOperations] = self.observationOperations
    def sequenceOperations: List[SequenceOperations] = self.sequenceOperations
  }

  implicit def operationsSyntax(a: SupportedOperations): SupportedOperationsOps =
    new SupportedOperationsOps(a)
}
