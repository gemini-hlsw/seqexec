// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import edu.gemini.seqexec.model.Model._
import java.time.Instant

import dhs.ImageFileId
import cats.Eq
import cats.implicits._

object events {
  implicit val instantEq: Eq[Instant] = Eq.fromUniversalEquals

  sealed trait SeqexecEvent
  sealed trait SeqexecModelUpdate extends SeqexecEvent {
    def view: SequencesQueue[SequenceView]
  }

  sealed trait ForClient extends SeqexecEvent {
    def clientId: ClientID
  }

  object SeqexecModelUpdate {
    import SeqexecEvent._
    implicit val equal: Eq[SeqexecModelUpdate] =
      Eq[Either[SequenceStart,
        Either[StepExecuted,
        Either[FileIdStepExecuted,
        Either[SequenceCompleted,
        Either[SequenceLoaded,
        Either[SequenceUnloaded,
        Either[StepBreakpointChanged,
        Either[OperatorUpdated,
        Either[ObserverUpdated,
        Either[ConditionsUpdated,
        Either[StepSkipMarkChanged,
        Either[SequencePauseRequested,
        Either[SequencePauseCanceled,
        Either[SequenceRefreshed,
        Either[ActionStopRequested,
        Either[ResourcesBusy,
        Either[SequenceUpdated,
        Either[SequencePaused,
        Either[ExposurePaused, SequenceError]]]]]]]]]]]]]]]]]]]].contramap {
        case s: SequenceStart          => Left(s)
        case s: StepExecuted           => Right(Left(s))
        case s: FileIdStepExecuted     => Right(Right(Left(s)))
        case s: SequenceCompleted      => Right(Right(Right(Left(s))))
        case s: SequenceLoaded         => Right(Right(Right(Right(Left(s)))))
        case s: SequenceUnloaded       => Right(Right(Right(Right(Right(Left(s))))))
        case s: StepBreakpointChanged  => Right(Right(Right(Right(Right(Right(Left(s)))))))
        case s: OperatorUpdated        => Right(Right(Right(Right(Right(Right(Right(Left(s))))))))
        case s: ObserverUpdated        => Right(Right(Right(Right(Right(Right(Right(Right(Left(s)))))))))
        case s: ConditionsUpdated      => Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s))))))))))
        case s: StepSkipMarkChanged    => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s)))))))))))
        case s: SequencePauseRequested => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s))))))))))))
        case s: SequencePauseCanceled  => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s)))))))))))))
        case s: SequenceRefreshed      => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s))))))))))))))
        case s: ActionStopRequested    => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s)))))))))))))))
        case s: ResourcesBusy          => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s))))))))))))))))
        case s: SequenceUpdated        => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s)))))))))))))))))
        case s: SequencePaused         => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s))))))))))))))))))
        case s: ExposurePaused         => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Left(s)))))))))))))))))))
        case s: SequenceError          => Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(Right(s)))))))))))))))))))
      }

    def unapply(u: SeqexecModelUpdate): Option[SequencesQueue[SequenceView]] =
      Some(u.view)
  }

  // scalastyle:off
  object SeqexecEvent {
    final case class ConnectionOpenEvent(u: Option[UserDetails], clientId: ClientID) extends SeqexecEvent

    object ConnectionOpenEvent {
      implicit val equal: Eq[ConnectionOpenEvent] = Eq.fromUniversalEquals
    }

    final case class SequenceStart(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceStart {
      implicit val equal: Eq[SequenceStart] =
        Eq.by(_.view)
    }

    final case class StepExecuted(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object StepExecuted {
      implicit val equal: Eq[StepExecuted] =
        Eq[(SequenceId, SequencesQueue[SequenceView])].contramap { x =>
          (x.obsId, x.view)
        }
    }

    final case class FileIdStepExecuted(fileId: ImageFileId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object FileIdStepExecuted{
      implicit val equal: Eq[FileIdStepExecuted] =
        Eq[(ImageFileId, SequencesQueue[SequenceView])].contramap { x =>
          (x.fileId, x.view)
        }
    }

    final case class SequenceCompleted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceCompleted {
      implicit val equal: Eq[SequenceCompleted] =
        Eq.by(_.view)
    }

    final case class SequenceLoaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceLoaded {
      implicit val equal: Eq[SequenceLoaded] =
        Eq[(SequenceId, SequencesQueue[SequenceView])].contramap { x =>
          (x.obsId, x.view)
        }
    }

    final case class SequenceUnloaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceUnloaded {
      implicit val equal: Eq[SequenceUnloaded] =
        Eq[(SequenceId, SequencesQueue[SequenceView])].contramap { x =>
          (x.obsId, x.view)
        }
    }

    final case class StepBreakpointChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object StepBreakpointChanged {
      implicit val equal: Eq[StepBreakpointChanged] =
        Eq.by(_.view)
    }

    final case class OperatorUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object OperatorUpdated {
      implicit val equal: Eq[OperatorUpdated] =
        Eq.by(_.view)
    }

    final case class ObserverUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object ObserverUpdated {
      implicit val equal: Eq[ObserverUpdated] =
        Eq.by(_.view)
    }

    final case class ConditionsUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object ConditionsUpdated {
      implicit val equal: Eq[ConditionsUpdated] =
        Eq.by(_.view)
    }

    final case class StepSkipMarkChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object StepSkipMarkChanged {
      implicit val equal: Eq[StepSkipMarkChanged] =
        Eq.by(_.view)
    }

    final case class SequencePauseRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequencePauseRequested {
      implicit val equal: Eq[SequencePauseRequested] =
        Eq.by(_.view)
    }

    final case class SequencePauseCanceled(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequencePauseCanceled {
      implicit val equal: Eq[SequencePauseCanceled] =
        Eq.by(_.view)
    }

    final case class SequenceRefreshed(view: SequencesQueue[SequenceView], clientId: ClientID) extends SeqexecModelUpdate with ForClient

    object SequenceRefreshed {
      implicit val equal: Eq[SequenceRefreshed] =
        Eq[(SequencesQueue[SequenceView], ClientID)].contramap(x => (x.view, x.clientId))
    }

    final case class ActionStopRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object ActionStopRequested {
      implicit val equal: Eq[ActionStopRequested] =
        Eq.by(_.view)
    }

    final case class ResourcesBusy(obsId: SequenceId, view: SequencesQueue[SequenceView], clientId: ClientID) extends SeqexecModelUpdate with ForClient

    object ResourcesBusy {
      implicit val equal: Eq[ResourcesBusy] =
        Eq[(SequenceId, SequencesQueue[SequenceView], ClientID)].contramap { x =>
          (x.obsId, x.view, x.clientId)
        }
    }

    final case class SequenceUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceUpdated {
      implicit val equal: Eq[SequenceUpdated] =
        Eq.by(_.view)
    }

    final case class SequencePaused(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequencePaused {
      implicit val equal: Eq[SequencePaused] =
        Eq[(SequenceId, SequencesQueue[SequenceView])].contramap { x =>
          (x.obsId, x.view)
        }
    }

    final case class ExposurePaused(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object ExposurePaused {
      implicit val equal: Eq[ExposurePaused] =
        Eq[(SequenceId, SequencesQueue[SequenceView])].contramap { x =>
          (x.obsId, x.view)
        }
    }

    final case class SequenceError(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceError {
      implicit val equal: Eq[SequenceError] =
        Eq[(SequenceId, SequencesQueue[SequenceView])].contramap { x =>
          (x.obsId, x.view)
        }
    }

    // TODO: msg should be LogMsg but it does IO when getting a timestamp, it
    // has to be embedded in a `Task`
    final case class NewLogMessage(msg: String) extends SeqexecEvent

    object NewLogMessage {
      implicit val equal: Eq[NewLogMessage] = Eq.fromUniversalEquals
    }

    final case class ServerLogMessage(level: ServerLogLevel, timestamp: Instant, msg: String) extends SeqexecEvent
    object ServerLogMessage {
      implicit val equal: Eq[ServerLogMessage] =
        Eq[(ServerLogLevel, Instant, String)].contramap { x =>
          (x.level, x.timestamp, x.msg)
        }
    }

    case object NullEvent extends SeqexecEvent
    implicit val neEqual: Eq[NullEvent.type] = Eq.fromUniversalEquals

    implicit val equal: Eq[SeqexecEvent] =
      Eq[Either[ConnectionOpenEvent, Either[SeqexecModelUpdate, Either[NewLogMessage, Either[ServerLogMessage, NullEvent.type]]]]].contramap {
        case s: ConnectionOpenEvent => Left(s)
        case s: SeqexecModelUpdate => Right(Left(s))
        case s: NewLogMessage => Right(Right(Left(s)))
        case s: ServerLogMessage => Right(Right(Right(Left(s))))
        case s: NullEvent.type => Right(Right(Right(Right(s))))
      }
  }
  // scalastyle:on

}
