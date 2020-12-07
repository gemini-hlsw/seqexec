// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import java.time.Instant

import cats._
import cats.syntax.all._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum._

object events {
  implicit val instantEq: Eq[Instant] = Eq.fromUniversalEquals

  sealed trait SeqexecEvent extends Product with Serializable
  sealed trait SeqexecModelUpdate extends SeqexecEvent {
    def view: SequencesQueue[SequenceView]
  }

  /**
    * Events implementing ForClient will be delivered only to the given
    * clientId
    */
  sealed trait ForClient extends SeqexecEvent {
    def clientId: ClientId
  }

  final case class ObservationProgressEvent(progress: Progress)
      extends SeqexecEvent

  object ObservationProgressEvent {
    implicit lazy val equal: Eq[ObservationProgressEvent] = Eq.by(_.progress)
  }

  final case class ServerLogMessage(level:     ServerLogLevel,
                                    timestamp: Instant,
                                    msg:       String)
      extends SeqexecEvent
  object ServerLogMessage {
    private implicit val instantOrder: Order[Instant] =
      Order.by(_.getNano)
    implicit val serverLogMessageOrder: Order[ServerLogMessage] =
      Order.by(x => (x.level, x.timestamp, x.msg))
  }

  case object NullEvent extends SeqexecEvent
  implicit lazy val neEqual: Eq[NullEvent.type] = Eq.instance {
    case (NullEvent, NullEvent) => true
    case _                      => false
  }

  final case class ConnectionOpenEvent(userDetails:   Option[UserDetails],
                                       clientId:      ClientId,
                                       serverVersion: String)
      extends SeqexecEvent

  object ConnectionOpenEvent {
    implicit lazy val equal: Eq[ConnectionOpenEvent] =
      Eq.by(x => (x.userDetails, x.clientId, x.serverVersion))
  }

  object SeqexecModelUpdate {
    implicit lazy val equalSE: Eq[SeqexecModelUpdate] =
      Eq.instance {
        case (a: SequenceStart,               b: SequenceStart)               => a === b
        case (a: StepExecuted,                b: StepExecuted)                => a === b
        case (a: FileIdStepExecuted,          b: FileIdStepExecuted)          => a === b
        case (a: SequenceCompleted,           b: SequenceCompleted)           => a === b
        case (a: SequenceLoaded,              b: SequenceLoaded)              => a === b
        case (a: SequenceUnloaded,            b: SequenceUnloaded)            => a === b
        case (a: StepBreakpointChanged,       b: StepBreakpointChanged)       => a === b
        case (a: OperatorUpdated,             b: OperatorUpdated)             => a === b
        case (a: ObserverUpdated,             b: ObserverUpdated)             => a === b
        case (a: ConditionsUpdated,           b: ConditionsUpdated)           => a === b
        case (a: StepSkipMarkChanged,         b: StepSkipMarkChanged)         => a === b
        case (a: SequencePauseRequested,      b: SequencePauseRequested)      => a === b
        case (a: SequencePauseCanceled,       b: SequencePauseCanceled)       => a === b
        case (a: SequenceRefreshed,           b: SequenceRefreshed)           => a === b
        case (a: ActionStopRequested,         b: ActionStopRequested)         => a === b
        case (a: SequenceUpdated,             b: SequenceUpdated)             => a === b
        case (a: SequencePaused,              b: SequencePaused)              => a === b
        case (a: ExposurePaused,              b: ExposurePaused)              => a === b
        case (a: SequenceError,               b: SequenceError)               => a === b
        case (a: LoadSequenceUpdated,         b: LoadSequenceUpdated)         => a === b
        case (a: ClearLoadedSequencesUpdated, b: ClearLoadedSequencesUpdated) => a === b
        case (a: QueueUpdated,                b: QueueUpdated)                => a === b
        case (a: SequenceStopped,             b: SequenceStopped)             => a === b
        case (a: SequenceAborted,             b: SequenceAborted)             => a === b
        case _                                                                => false
      }

    def unapply(u: SeqexecModelUpdate): Option[SequencesQueue[SequenceView]] =
      Some(u.view)
  }

  final case class SequenceStart(obsId: Observation.Id, stepId: StepId, view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequenceStart {
    implicit lazy val equal: Eq[SequenceStart] =
      Eq.by(x => (x.obsId, x.stepId, x.view))
  }

  final case class StepExecuted(obsId: Observation.Id,
                                view:  SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object StepExecuted {
    implicit lazy val equal: Eq[StepExecuted] =
      Eq.by(x => (x.obsId, x.view))
  }

  final case class FileIdStepExecuted(fileId: ImageFileId,
                                      view:   SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object FileIdStepExecuted {
    implicit lazy val equal: Eq[FileIdStepExecuted] =
      Eq.by(x => (x.fileId, x.view))
  }

  final case class SequenceCompleted(view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequenceCompleted {
    implicit lazy val equal: Eq[SequenceCompleted] =
      Eq.by(_.view)
  }

  final case class SequenceLoaded(obsId: Observation.Id,
                                  view:  SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequenceLoaded {
    implicit lazy val equal: Eq[SequenceLoaded] =
      Eq.by(x => (x.obsId, x.view))
  }

  final case class SequenceUnloaded(obsId: Observation.Id,
                                    view:  SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequenceUnloaded {
    implicit lazy val equal: Eq[SequenceUnloaded] =
      Eq.by(x => (x.obsId, x.view))
  }

  final case class StepBreakpointChanged(view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object StepBreakpointChanged {
    implicit lazy val equal: Eq[StepBreakpointChanged] =
      Eq.by(_.view)
  }

  final case class OperatorUpdated(view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object OperatorUpdated {
    implicit lazy val equal: Eq[OperatorUpdated] =
      Eq.by(_.view)
  }

  final case class QueueUpdated(op:   QueueManipulationOp,
                                view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object QueueUpdated {
    implicit lazy val equal: Eq[QueueUpdated] =
      Eq.by(x => (x.op, x.view))
  }

  final case class SingleActionEvent(op: SingleActionOp) extends SeqexecEvent

  object SingleActionEvent {
    implicit lazy val equal: Eq[SingleActionEvent] =
      Eq.by(_.op)
  }

  final case class LoadSequenceUpdated(i:        Instrument,
                                       sid:      Observation.Id,
                                       view:     SequencesQueue[SequenceView],
                                       clientId: ClientId)
      extends SeqexecModelUpdate

  object LoadSequenceUpdated {
    implicit lazy val equal: Eq[LoadSequenceUpdated] =
      Eq.by(x => (x.i, x.sid, x.view, x.clientId))
  }

  final case class ClearLoadedSequencesUpdated(
    view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object ClearLoadedSequencesUpdated {
    implicit lazy val clsEqual: Eq[ClearLoadedSequencesUpdated] =
      Eq.by(_.view)
  }

  final case class ObserverUpdated(view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object ObserverUpdated {
    implicit lazy val equal: Eq[ObserverUpdated] =
      Eq.by(_.view)
  }

  final case class ConditionsUpdated(view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object ConditionsUpdated {
    implicit lazy val equal: Eq[ConditionsUpdated] =
      Eq.by(_.view)
  }

  final case class StepSkipMarkChanged(view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object StepSkipMarkChanged {
    implicit lazy val equal: Eq[StepSkipMarkChanged] =
      Eq.by(_.view)
  }

  final case class SequencePauseRequested(view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequencePauseRequested {
    implicit lazy val equal: Eq[SequencePauseRequested] =
      Eq.by(_.view)
  }

  final case class SequencePauseCanceled(obsId: Observation.Id,
                                         view:  SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequencePauseCanceled {
    implicit lazy val equal: Eq[SequencePauseCanceled] =
      Eq.by(x => (x.obsId, x.view))
  }

  final case class SequenceRefreshed(view:     SequencesQueue[SequenceView],
                                     clientId: ClientId)
      extends SeqexecModelUpdate
      with ForClient

  object SequenceRefreshed {
    implicit lazy val equal: Eq[SequenceRefreshed] =
      Eq.by(x => (x.view, x.clientId))
  }

  final case class ActionStopRequested(view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object ActionStopRequested {
    implicit lazy val equal: Eq[ActionStopRequested] =
      Eq.by(_.view)
  }

  final case class SequenceStopped(obsId: Observation.Id,
                                   view:  SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequenceStopped {
    implicit lazy val equal: Eq[SequenceStopped] =
      Eq.by(x => (x.obsId, x.view))
  }

  final case class SequenceAborted(obsId: Observation.Id,
                                   view:  SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequenceAborted {
    implicit lazy val equal: Eq[SequenceAborted] =
      Eq.by(x => (x.obsId, x.view))
  }

  final case class SequenceUpdated(view: SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequenceUpdated {
    implicit lazy val equal: Eq[SequenceUpdated] =
      Eq.by(_.view)
  }

  final case class SequencePaused(obsId: Observation.Id,
                                  view:  SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequencePaused {
    implicit lazy val equal: Eq[SequencePaused] =
      Eq.by(x => (x.obsId, x.view))
  }

  final case class ExposurePaused(obsId: Observation.Id,
                                  view:  SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object ExposurePaused {
    implicit lazy val equal: Eq[ExposurePaused] =
      Eq.by(x => (x.obsId, x.view))
  }

  final case class SequenceError(obsId: Observation.Id,
                                 view:  SequencesQueue[SequenceView])
      extends SeqexecModelUpdate

  object SequenceError {
    implicit lazy val equal: Eq[SequenceError] =
      Eq.by(x => (x.obsId, x.view))
  }

  final case class UserNotification(memo: Notification, clientId: ClientId)
      extends ForClient

  object UserNotification {
    implicit lazy val equal: Eq[UserNotification] =
      Eq.by(x => (x.memo, x.clientId))
  }

  final case class UserPromptNotification(prompt: UserPrompt, clientId: ClientId)
      extends ForClient

  object UserPromptNotification {
    implicit lazy val equal: Eq[UserPromptNotification] =
      Eq.by(x => (x.prompt, x.clientId))
  }

  final case class GuideConfigUpdate(telescope: TelescopeGuideConfig)
      extends SeqexecEvent

  object GuideConfigUpdate {
    implicit lazy val equal: Eq[GuideConfigUpdate] =
      Eq.by(_.telescope)
  }

  final case class AlignAndCalibEvent(step: Int)
      extends SeqexecEvent

  object AlignAndCalibEvent {
    implicit lazy val equal: Eq[AlignAndCalibEvent] =
      Eq.by(_.step)
  }

  implicit val equal: Eq[SeqexecEvent] =
    Eq.instance {
      case (a: ConnectionOpenEvent,      b: ConnectionOpenEvent)      => a === b
      case (a: SeqexecModelUpdate,       b: SeqexecModelUpdate)       => a === b
      case (a: ServerLogMessage,         b: ServerLogMessage)         => a === b
      case (a: UserNotification,         b: UserNotification)         => a === b
      case (a: UserPromptNotification,   b: UserPromptNotification)   => a === b
      case (a: GuideConfigUpdate,        b: GuideConfigUpdate)        => a === b
      case (a: ObservationProgressEvent, b: ObservationProgressEvent) => a === b
      case (a: SingleActionEvent,        b: SingleActionEvent)        => a === b
      case (a: AlignAndCalibEvent,       b: AlignAndCalibEvent)       => a === b
      case (_: NullEvent.type,           _: NullEvent.type)           => true
      case _                                                          => false
    }

}
