// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import seqexec.model.enum.Instrument
import seqexec.model.enum.Instrument._

object operations {
  sealed trait OperationLevel
  object OperationLevel {
    sealed trait Observation extends OperationLevel
    sealed trait NsCycle extends OperationLevel
    sealed trait NsNod extends OperationLevel
  }

  import OperationLevel._

  sealed trait Operations[A <: OperationLevel]
  object Operations {
    // Operations possible at the observation level
    case object PauseObservation extends Operations[Observation]
    case object StopObservation extends Operations[Observation]
    case object AbortObservation extends Operations[Observation]
    case object ResumeObservation extends Operations[Observation]

    // Operations possible for N&S Cycle
    case object PauseGracefullyObservation extends Operations[NsCycle]
    case object StopGracefullyObservation extends Operations[NsCycle]

    // Operations possible for N&S Nod
    case object PauseImmediatelyObservation extends Operations[NsNod]
    case object StopImmediatelyObservation extends Operations[NsNod]
  }

  sealed trait OperationLevelType[L <: OperationLevel]
  implicit object ObservationLevel extends OperationLevelType[Observation]
  implicit object NsCycleLevel extends OperationLevelType[NsCycle]
  implicit object NsNodLevel extends OperationLevelType[NsNod]

  sealed trait SupportedOperations {
    def apply[L <: OperationLevel](isObservePaused: Boolean, isMultiLevel: Boolean)
      (implicit level: OperationLevelType[L]): List[Operations[L]]
  }

  private val F2SupportedOperations = new SupportedOperations {
    def apply[L <: OperationLevel](isObservePaused: Boolean, isMultiLevel: Boolean)
      (implicit level: OperationLevelType[L]): List[Operations[L]] =
      Nil
  }

  private val GmosSupportedOperations = new SupportedOperations {
    def apply[L <: OperationLevel](isObservePaused: Boolean, isMultiLevel: Boolean)
      (implicit level: OperationLevelType[L]): List[Operations[L]] =
      level match {
        case ObservationLevel =>
          if(isMultiLevel) {
            if (isObservePaused) {
              List(Operations.ResumeObservation,
                   Operations.AbortObservation)
            } else {
              List(Operations.AbortObservation)
            }
          } else {
            if (isObservePaused) {
              List(Operations.ResumeObservation,
                   Operations.StopObservation,
                   Operations.AbortObservation)
            } else {
              List(Operations.PauseObservation,
                   Operations.StopObservation,
                   Operations.AbortObservation)
            }
          }
        case NsCycleLevel =>
            List(Operations.PauseGracefullyObservation,
                 Operations.StopGracefullyObservation)
        case NsNodLevel =>
          List(Operations.PauseImmediatelyObservation,
               Operations.StopImmediatelyObservation)
        case _                => Nil
      }
  }

  private val GnirsSupportedOperations = new SupportedOperations {
    def apply[L <: OperationLevel](isObservePaused: Boolean, isMultiLevel: Boolean)
      (implicit level: OperationLevelType[L]): List[Operations[L]] =
      level match {
        case ObservationLevel =>
          if (isObservePaused) {
            List(Operations.StopObservation,
                 Operations.AbortObservation)
          } else {
            List(Operations.StopObservation,
                 Operations.AbortObservation)
          }
        case _                => Nil
      }
  }

  private val NiriSupportedOperations = new SupportedOperations {
    def apply[L <: OperationLevel](isObservePaused: Boolean, isMultiLevel: Boolean)
      (implicit level: OperationLevelType[L]): List[Operations[L]] =
      level match {
        case ObservationLevel =>
          if (isObservePaused) {
            List(Operations.StopObservation,
                 Operations.AbortObservation)
          } else {
            List(Operations.StopObservation,
                 Operations.AbortObservation)
          }
        case _                => Nil
      }
  }

  private val NifsSupportedOperations = new SupportedOperations {
    def apply[L <: OperationLevel](isObservePaused: Boolean, isMultiLevel: Boolean)
      (implicit level: OperationLevelType[L]): List[Operations[L]] =
      level match {
        case ObservationLevel =>
          if (isObservePaused) {
            List(Operations.StopObservation,
                 Operations.AbortObservation)
          } else {
            List(Operations.StopObservation,
                 Operations.AbortObservation)
          }
        case _                => Nil
      }
  }

  private val GsaoiSupportedOperations = new SupportedOperations {
    def apply[L <: OperationLevel](isObservePaused: Boolean, isMultiLevel: Boolean)
      (implicit level: OperationLevelType[L]): List[Operations[L]] =
      level match {
        case ObservationLevel =>
          List(Operations.StopObservation,
               Operations.AbortObservation)
        case _                => Nil
      }
  }

  private val NilSupportedOperations = new SupportedOperations {
    def apply[L <: OperationLevel](isObservePaused: Boolean, isMultiLevel: Boolean)
      (implicit level: OperationLevelType[L]): List[Operations[L]] =
      Nil
  }

  private val instrumentOperations: Map[Instrument, SupportedOperations] = Map(
    F2    -> F2SupportedOperations,
    GmosS -> GmosSupportedOperations,
    GmosN -> GmosSupportedOperations,
    Gnirs -> GnirsSupportedOperations,
    Niri  -> NiriSupportedOperations,
    Nifs  -> NifsSupportedOperations,
    Gsaoi -> GsaoiSupportedOperations
  )

  final implicit class SupportedOperationsOps(val i: Instrument)
      extends AnyVal {
    def operations[L <: OperationLevel](isObservePaused: Boolean, isMultiLevel: Boolean = false)
      (implicit level: OperationLevelType[L]): List[Operations[L]] =
      instrumentOperations
        .getOrElse(i, NilSupportedOperations)(isObservePaused, isMultiLevel)
  }

}
