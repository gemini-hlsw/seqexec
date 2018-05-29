// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import diode.Action
import seqexec.model.UserDetails
import seqexec.model.Model._
import seqexec.model.events._
import seqexec.web.client.model._
import org.scalajs.dom.WebSocket

import cats.Show

object actions {

  // scalastyle:off
  // Actions
  final case class NavigateTo(page: Pages.SeqexecPages) extends Action
  final case class NavigateSilentTo(page: Pages.SeqexecPages) extends Action
  final case class Initialize(site: SeqexecSite) extends Action

  // Actions to close and/open the login box
  case object OpenLoginBox extends Action
  case object CloseLoginBox extends Action
  case object OpenResourcesBox extends Action
  case object CloseResourcesBox extends Action
  final case class SequenceInConflict(id: SequenceId) extends Action

  final case class LoggedIn(u: UserDetails) extends Action
  case object Logout extends Action

  // Action to select a sequence for display
  final case class SelectIdToDisplay(id: SequenceId) extends Action
  final case class SelectInstrumentToDisplay(i: Instrument) extends Action
  final case class SelectSequenceConfig(id: SequenceId, step: Int) extends Action

  // Actions related to executing sequences
  final case class RequestRun(s: SequenceId) extends Action
  final case class RequestSync(s: SequenceId) extends Action
  final case class RequestPause(s: SequenceId) extends Action
  final case class RequestCancelPause(s: SequenceId) extends Action
  final case class RequestStop(id: SequenceId, step: Int) extends Action
  final case class RequestAbort(id: SequenceId, step: Int) extends Action
  final case class RequestObsPause(id: SequenceId, step: Int) extends Action
  final case class RequestObsResume(id: SequenceId, step: Int) extends Action
  case object RequestSoundEcho extends Action

  final case class RunStarted(s: SequenceId) extends Action
  final case class RunPaused(s: SequenceId) extends Action
  final case class RunCancelPaused(s: SequenceId) extends Action
  final case class RunSync(s: SequenceId) extends Action
  final case class RunStartFailed(s: SequenceId) extends Action
  final case class RunPauseFailed(s: SequenceId) extends Action
  final case class RunCancelPauseFailed(s: SequenceId) extends Action
  final case class RunSyncFailed(s: SequenceId) extends Action
  final case class RunStop(s: SequenceId) extends Action
  final case class RunStopFailed(s: SequenceId) extends Action
  final case class RunAbort(s: SequenceId) extends Action
  final case class RunAbortFailed(s: SequenceId) extends Action
  final case class RunObsPause(s: SequenceId) extends Action
  final case class RunObsPauseFailed(s: SequenceId) extends Action
  final case class RunObsResumeFailed(s: SequenceId) extends Action

  final case class ShowStepConfig(id: SequenceId, step: Int) extends Action
  final case class HideStepConfig(i: Instrument) extends Action
  final case class RememberCompleted(s: SequenceView) extends Action

  final case class AppendToLog(l: ServerLogMessage) extends Action
  final case object ToggleLogArea extends Action

  // Actions related to web sockets
  final case class WSConnect(delay: Int) extends Action
  case object Reconnect extends Action
  case object Connecting extends Action
  final case class Connected(ws: WebSocket, delay: Int) extends Action
  final case class ConnectionRetry(delay: Int) extends Action
  final case class ConnectionError(s: String) extends Action
  final case class ServerMessage(e: SeqexecEvent) extends Action

  // Temporal actions for UI prototyping
  final case class FlipSkipStep(id: SequenceId, step: Step) extends Action
  final case class FlipBreakpointStep(id: SequenceId, step: Step) extends Action
  final case class UpdateObserver(id: SequenceId, name: String) extends Action
  final case class UpdateOperator(name: Operator) extends Action
  final case class UpdateImageQuality(iq: ImageQuality) extends Action
  final case class UpdateCloudCover(cc: CloudCover) extends Action
  final case class UpdateSkyBackground(sb: SkyBackground) extends Action
  final case class UpdateWaterVapor(wv: WaterVapor) extends Action

  // scalastyle:on
  private val standardStep: PartialFunction[Step, (StepId, StepState, List[(Resource, ActionStatus)])] = {
    case i: StandardStep => (i.id, i.status, i.configStatus)
  }

  implicit val show: Show[Action] = Show.show {
    case s @ ServerMessage(u @ SeqexecModelUpdate(view)) =>
      s"${s.getClass.getSimpleName}(${u.getClass.getSimpleName}(${view.queue.map(s => (s.id, s.steps.collect(standardStep)))}))"
    case s @ RememberCompleted(view)                     =>
      s"${s.getClass.getSimpleName}(${view.id})"
    case a                                               =>
      s"$a"
  }
}
