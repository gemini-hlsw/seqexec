// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import seqexec.model.enum._
import java.time.Instant

import dhs.ImageFileId
import cats.Eq
import cats.implicits._
import gem.Observation

object events {
  implicit val instantEq: Eq[Instant] = Eq.fromUniversalEquals

  sealed trait SeqexecEvent extends Product with Serializable
  sealed trait SeqexecModelUpdate extends SeqexecEvent {
    def view: SequencesQueue[SequenceView]
  }

  sealed trait ForClient extends SeqexecEvent {
    def clientId: ClientID
  }

  // TODO: msg should be LogMsg but it does IO when getting a timestamp, it
  // has to be embedded in a `Task`
  final case class NewLogMessage(msg: String) extends SeqexecEvent

  object NewLogMessage {
    implicit lazy val equal: Eq[NewLogMessage] = Eq.fromUniversalEquals
  }

  final case class ServerLogMessage(level: ServerLogLevel, timestamp: Instant, msg: String) extends SeqexecEvent
  object ServerLogMessage {
    implicit lazy val equal: Eq[ServerLogMessage] =
      Eq.by(x => (x.level, x.timestamp, x.msg))
  }

  case object NullEvent extends SeqexecEvent
  implicit lazy val neEqual: Eq[NullEvent.type] = Eq.instance {
    case (NullEvent, NullEvent) => true
    case _                      => false
  }

  final case class ConnectionOpenEvent(u: Option[UserDetails], clientId: ClientID) extends SeqexecEvent

  object ConnectionOpenEvent {
    implicit lazy val equal: Eq[ConnectionOpenEvent] =
      Eq.by(x => (x.u, x.clientId))
  }

  object SeqexecModelUpdate {
    implicit lazy val equalSE: Eq[SeqexecModelUpdate] =
      Eq.instance {
        case (a: SequenceStart,           b: SequenceStart)           => a === b
        case (a: StepExecuted,            b: StepExecuted)            => a === b
        case (a: FileIdStepExecuted,      b: FileIdStepExecuted)      => a === b
        case (a: SequenceCompleted,       b: SequenceCompleted)       => a === b
        case (a: SequenceLoaded,          b: SequenceLoaded)          => a === b
        case (a: SequenceUnloaded,        b: SequenceUnloaded)        => a === b
        case (a: StepBreakpointChanged,   b: StepBreakpointChanged)   => a === b
        case (a: OperatorUpdated,         b: OperatorUpdated)         => a === b
        case (a: ObserverUpdated,         b: ObserverUpdated)         => a === b
        case (a: ConditionsUpdated,       b: ConditionsUpdated)       => a === b
        case (a: StepSkipMarkChanged,     b: StepSkipMarkChanged)     => a === b
        case (a: SequencePauseRequested,  b: SequencePauseRequested)  => a === b
        case (a: SequencePauseCanceled,   b: SequencePauseCanceled)   => a === b
        case (a: SequenceRefreshed,       b: SequenceRefreshed)       => a === b
        case (a: ActionStopRequested,     b: ActionStopRequested)     => a === b
        case (a: ResourcesBusy,           b: ResourcesBusy)           => a === b
        case (a: SequenceUpdated,         b: SequenceUpdated)         => a === b
        case (a: SequencePaused,          b: SequencePaused)          => a === b
        case (a: ExposurePaused,          b: ExposurePaused)          => a === b
        case (a: SequenceError,           b: SequenceError)           => a === b
        case _                                                        => false
      }

    def unapply(u: SeqexecModelUpdate): Option[SequencesQueue[SequenceView]] =
      Some(u.view)
  }

  // scalastyle:off
    final case class SequenceStart(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceStart {
      implicit lazy val equal: Eq[SequenceStart] =
        Eq.by(_.view)
    }

    final case class StepExecuted(obsId: Observation.Id, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object StepExecuted {
      implicit lazy val equal: Eq[StepExecuted] =
        Eq.by(x => (x.obsId, x.view))
    }

    final case class FileIdStepExecuted(fileId: ImageFileId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object FileIdStepExecuted{
      implicit lazy val equal: Eq[FileIdStepExecuted] =
        Eq.by(x => (x.fileId, x.view))
    }

    final case class SequenceCompleted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceCompleted {
      implicit lazy val equal: Eq[SequenceCompleted] =
        Eq.by(_.view)
    }

    final case class SequenceLoaded(obsId: Observation.Id, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceLoaded {
      implicit lazy val equal: Eq[SequenceLoaded] =
        Eq.by(x => (x.obsId, x.view))
    }

    final case class SequenceUnloaded(obsId: Observation.Id, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceUnloaded {
      implicit lazy val equal: Eq[SequenceUnloaded] =
        Eq.by(x => (x.obsId, x.view))
    }

    final case class StepBreakpointChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object StepBreakpointChanged {
      implicit lazy val equal: Eq[StepBreakpointChanged] =
        Eq.by(_.view)
    }

    final case class OperatorUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object OperatorUpdated {
      implicit lazy val equal: Eq[OperatorUpdated] =
        Eq.by(_.view)
    }

    final case class LoadSequenceUpdated(i: Instrument, sid: Observation.Id) extends SeqexecEvent

    object LoadSequenceUpdated {
      implicit lazy val equal: Eq[LoadSequenceUpdated] =
        Eq.by(x => (x.i, x.sid))
    }

    case object ClearLoadedSequencesUpdated extends SeqexecEvent

    implicit lazy val clsEqual: Eq[ClearLoadedSequencesUpdated.type] = Eq.fromUniversalEquals

    final case class ObserverUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object ObserverUpdated {
      implicit lazy val equal: Eq[ObserverUpdated] =
        Eq.by(_.view)
    }

    final case class ConditionsUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object ConditionsUpdated {
      implicit lazy val equal: Eq[ConditionsUpdated] =
        Eq.by(_.view)
    }

    final case class StepSkipMarkChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object StepSkipMarkChanged {
      implicit lazy val equal: Eq[StepSkipMarkChanged] =
        Eq.by(_.view)
    }

    final case class SequencePauseRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequencePauseRequested {
      implicit lazy val equal: Eq[SequencePauseRequested] =
        Eq.by(_.view)
    }

    final case class SequencePauseCanceled(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequencePauseCanceled {
      implicit lazy val equal: Eq[SequencePauseCanceled] =
        Eq.by(_.view)
    }

    final case class SequenceRefreshed(view: SequencesQueue[SequenceView], clientId: ClientID) extends SeqexecModelUpdate with ForClient

    object SequenceRefreshed {
      implicit lazy val equal: Eq[SequenceRefreshed] =
        Eq.by(x => (x.view, x.clientId))
    }

    final case class ActionStopRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object ActionStopRequested {
      implicit lazy val equal: Eq[ActionStopRequested] =
        Eq.by(_.view)
    }

    final case class ResourcesBusy(obsId: Observation.Id, view: SequencesQueue[SequenceView], clientId: ClientID) extends SeqexecModelUpdate with ForClient

    object ResourcesBusy {
      implicit lazy val equal: Eq[ResourcesBusy] =
        Eq.by(x => (x.obsId, x.view, x.clientId))
    }

    final case class SequenceUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceUpdated {
      implicit lazy val equal: Eq[SequenceUpdated] =
        Eq.by(_.view)
    }

    final case class SequencePaused(obsId: Observation.Id, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequencePaused {
      implicit lazy val equal: Eq[SequencePaused] =
        Eq.by(x => (x.obsId, x.view))
    }

    final case class ExposurePaused(obsId: Observation.Id, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object ExposurePaused {
      implicit lazy val equal: Eq[ExposurePaused] =
        Eq.by(x => (x.obsId, x.view))
    }

    final case class SequenceError(obsId: Observation.Id, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceError {
      implicit lazy val equal: Eq[SequenceError] =
        Eq.by(x => (x.obsId, x.view))
    }

    implicit val equal: Eq[SeqexecEvent] =
      Eq.instance {
        case (a: ConnectionOpenEvent,              b: ConnectionOpenEvent)              => a === b
        case (a: SeqexecModelUpdate,               b: SeqexecModelUpdate)               => a === b
        case (a: NewLogMessage,                    b: NewLogMessage)                    => a === b
        case (a: ServerLogMessage,                 b: ServerLogMessage)                 => a === b
        case (a: LoadSequenceUpdated,              b: LoadSequenceUpdated)              => a === b
        case (_: ClearLoadedSequencesUpdated.type, _: ClearLoadedSequencesUpdated.type) => true
        case (_: NullEvent.type,                   _: NullEvent.type)                   => true
        case _                                                                          => false
      }

  // scalastyle:on

}
