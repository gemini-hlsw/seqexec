// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import diode.Action
import cats.Show
import cats.implicits._
import gem.Observation
import gem.enum.Site
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.events._
import seqexec.web.client.model.Pages._
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.SessionQueueTableBody
import seqexec.web.client.components.sequence.steps.StepsTable
import org.scalajs.dom.WebSocket
import web.client.table._

object actions {

  // scalastyle:off
  // Actions
  final case class NavigateTo(page:       SeqexecPages) extends Action
  final case class NavigateSilentTo(page: SeqexecPages) extends Action
  final case class Initialize(site:       Site) extends Action

  // Actions to close and/open the login box
  case object OpenLoginBox extends Action
  case object CloseLoginBox extends Action
  case object OpenUserNotificationBox extends Action
  case object CloseUserNotificationBox extends Action

  final case class LoggedIn(u: UserDetails) extends Action
  case object Logout extends Action

  // Action to select a sequence for display
  final case class SelectIdToDisplay(i:    Instrument,
                                     id:   Observation.Id,
                                     step: StepIdDisplayed)
      extends Action
  final case class SelectSequencePreview(i:    Instrument,
                                         id:   Observation.Id,
                                         step: StepIdDisplayed)
      extends Action
  case object SelectCalibrationQueue extends Action
  case object SelectRoot extends Action
  final case class ShowStepConfig(i: Instrument, id: Observation.Id, step: Int)
      extends Action
  final case class ShowPreviewStepConfig(i:    Instrument,
                                         id:   Observation.Id,
                                         step: Int)
      extends Action

  // Actions related to executing sequences
  final case class RequestRun(s:         Observation.Id) extends Action
  final case class RequestSync(s:        Observation.Id) extends Action
  final case class RequestPause(s:       Observation.Id) extends Action
  final case class RequestCancelPause(s: Observation.Id) extends Action
  final case class RequestStop(id:       Observation.Id, step: Int) extends Action
  final case class RequestAbort(id:      Observation.Id, step: Int) extends Action
  final case class RequestObsPause(id:   Observation.Id, step: Int) extends Action
  final case class RequestObsResume(id:  Observation.Id, step: Int) extends Action
  case object RequestSoundEcho extends Action

  final case class RunStarted(s:           Observation.Id) extends Action
  final case class RunPaused(s:            Observation.Id) extends Action
  final case class RunCancelPaused(s:      Observation.Id) extends Action
  final case class RunSync(s:              Observation.Id) extends Action
  final case class RunStartFailed(s:       Observation.Id) extends Action
  final case class RunPauseFailed(s:       Observation.Id) extends Action
  final case class RunCancelPauseFailed(s: Observation.Id) extends Action
  final case class RunSyncFailed(s:        Observation.Id) extends Action
  final case class RunStop(s:              Observation.Id) extends Action
  final case class RunStopFailed(s:        Observation.Id) extends Action
  final case class RunAbort(s:             Observation.Id) extends Action
  final case class RunAbortFailed(s:       Observation.Id) extends Action
  final case class RunObsPause(s:          Observation.Id) extends Action
  final case class RunObsPauseFailed(s:    Observation.Id) extends Action
  final case class RunObsResumeFailed(s:   Observation.Id) extends Action

  // Queue actions
  final case class RequestAllDayCal(qid:   QueueId) extends Action
  final case class AllDayCalCompleted(qid: QueueId) extends Action
  final case class AllDayCalFailed(qid:    QueueId) extends Action

  final case class RememberCompleted(s: SequenceView) extends Action

  final case class AppendToLog(l: ServerLogMessage) extends Action
  final case object ToggleLogArea extends Action

  // Actions related to web sockets
  final case class WSConnect(delay: Int) extends Action
  case object WSClose extends Action
  case object Reconnect extends Action
  case object Connecting extends Action
  final case class Connected(ws:          WebSocket, delay: Int) extends Action
  final case class ConnectionRetry(delay: Int) extends Action
  final case class ConnectionError(s:     String) extends Action
  final case class ServerMessage(e:       SeqexecEvent) extends Action

  final case class FlipSkipStep(id:       Observation.Id, step: Step) extends Action
  final case class FlipBreakpointStep(id: Observation.Id, step: Step) extends Action

  final case class UpdateObserver(id: Observation.Id, name: Observer) extends Action
  final case class UpdateDefaultObserver(name: Observer) extends Action
  final case class UpdateOperator(name:        Operator) extends Action
  final case class UpdateImageQuality(iq:      ImageQuality) extends Action
  final case class UpdateCloudCover(cc:        CloudCover) extends Action
  final case class UpdateSkyBackground(sb:     SkyBackground) extends Action
  final case class UpdateWaterVapor(wv:        WaterVapor) extends Action

  final case class UpdateStepsConfigTableState(s: TableState[StepConfigTable.TableColumn])
      extends Action
  final case class UpdateSessionQueueTableState(s: TableState[SessionQueueTableBody.TableColumn])
      extends Action
  final case class UpdateStepTableState(id: Observation.Id,
                                        s:  TableState[StepsTable.TableColumn])
      extends Action
  final case class LoadSequence(observer: Observer,
                                i:        Instrument,
                                id:       Observation.Id)
      extends Action
  final case class SequenceLoadFailed(id:       Observation.Id) extends Action
  final case class RequestFailedNotification(r: RequestFailed) extends Action
  case object CleanSequences extends Action

  // Used for UI debugging
  final case class MarkStepAsRunning(s: Observation.Id, step: Int) extends Action

  // scalastyle:on
  private val standardStep: PartialFunction[Step, (StepId, StepState, List[(Resource, ActionStatus)])] = {
    case i: StandardStep => (i.id, i.status, i.configStatus)
  }

  implicit val show: Show[Action] = Show.show {
    case s @ ServerMessage(u @ SeqexecModelUpdate(view)) =>
      val someSteps = view.sessionQueue.map(
        s =>
          (s"id: ${s.id.format}",
           s"steps: ${s.steps.length}",
           s.steps
             .filter(_.status === StepState.Running)
             .slice(0, scala.math.min(s.steps.length, 20))
             .collect(standardStep)))
      val dayCalQueue = view.queues.values.map(_.queue).mkString(",")
      s"${s.getClass.getSimpleName}(${u.getClass.getSimpleName}, dayCal: '${dayCalQueue}', loaded: '${view.loaded.mkString(",")}', $someSteps)"
    case s @ RememberCompleted(view)                    =>
      s"${s.getClass.getSimpleName}(${view.id})"
    case a                                              =>
      s"$a"
  }
}
