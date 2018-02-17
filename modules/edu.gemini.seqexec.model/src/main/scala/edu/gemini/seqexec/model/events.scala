// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import edu.gemini.seqexec.model.Model._
import java.time.Instant
import dhs.ImageFileId
import cats.Eq

object events {

  sealed trait SeqexecEvent
  sealed trait SeqexecModelUpdate extends SeqexecEvent {
    def view: SequencesQueue[SequenceView]
  }

  sealed trait ForClient extends SeqexecEvent {
    def clientId: ClientID
  }

  object SeqexecModelUpdate {
    implicit val equal: Eq[SeqexecModelUpdate] = Eq.fromUniversalEquals

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
      implicit val equal: Eq[SequenceStart] = Eq.fromUniversalEquals
    }

    final case class StepExecuted(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object StepExecuted {
      implicit val equal: Eq[StepExecuted] = Eq.fromUniversalEquals
    }

    final case class FileIdStepExecuted(fileId: ImageFileId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceCompleted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceLoaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceUnloaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class StepBreakpointChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class OperatorUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ObserverUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ConditionsUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class StepSkipMarkChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequencePauseRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequencePauseCanceled(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceRefreshed(view: SequencesQueue[SequenceView], clientId: ClientID) extends SeqexecModelUpdate with ForClient

    final case class ActionStopRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ResourcesBusy(obsId: SequenceId, view: SequencesQueue[SequenceView], clientId: ClientID) extends SeqexecModelUpdate with ForClient

    final case class SequenceUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequencePaused(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ExposurePaused(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceError(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    // TODO: msg should be LogMsg but it does IO when getting a timestamp, it
    // has to be embedded in a `Task`
    final case class NewLogMessage(msg: String) extends SeqexecEvent
    final case class ServerLogMessage(level: ServerLogLevel, timestamp: Instant, msg: String) extends SeqexecEvent
    case object NullEvent extends SeqexecEvent

    implicit val equal: Eq[SeqexecEvent] = Eq.fromUniversalEquals
  }
  // scalastyle:on

}
