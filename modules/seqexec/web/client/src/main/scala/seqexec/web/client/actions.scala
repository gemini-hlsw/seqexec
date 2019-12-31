// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import seqexec.web.client.model.SessionQueueFilter
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.sequence.steps.StepsTable
import seqexec.web.client.components.SessionQueueTable
import seqexec.web.client.components.queue.CalQueueTable
import org.scalajs.dom.WebSocket
import pprint.PPrinter
import web.client.table._

object actions {

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
  case object VerifyLoggedStatus extends Action

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
  final case class ShowStepConfig(i: Instrument, id: Observation.Id, step: StepId)
      extends Action
  final case class ShowPreviewStepConfig(i:    Instrument,
                                         id:   Observation.Id,
                                         step: StepId)
      extends Action

  // Actions related to executing sequences
  final case class RequestRun(s:               Observation.Id) extends Action
  final case class RequestSync(s:              Observation.Id) extends Action
  final case class RequestPause(s:             Observation.Id) extends Action
  final case class RequestCancelPause(s:       Observation.Id) extends Action
  final case class RequestStop(id:             Observation.Id, step: StepId) extends Action
  final case class RequestGracefulStop(id:     Observation.Id, step: StepId) extends Action
  final case class RequestAbort(id:            Observation.Id, step: StepId) extends Action
  final case class RequestObsPause(id:         Observation.Id, step: StepId) extends Action
  final case class RequestGracefulObsPause(id: Observation.Id, step: StepId) extends Action
  final case class RequestObsResume(id:        Observation.Id, step: StepId) extends Action
  case object RequestSoundEcho extends Action

  final case class RequestResourceRun(id:       Observation.Id,
                                      step:     StepId,
                                      resource: Resource)
      extends Action
  final case class RunResource(id:       Observation.Id,
                               step:     StepId,
                               resource: Resource)
      extends Action
  final case class RunResourceRemote(id:       Observation.Id,
                                     step:     StepId,
                                     resource: Resource)
      extends Action
  final case class RunResourceComplete(id:       Observation.Id,
                                       step:     StepId,
                                       resource: Resource)
      extends Action
  final case class RunResourceFailed(id:       Observation.Id,
                                     step:     StepId,
                                     resource: Resource,
                                     msg:      String)
      extends Action
  final case class RequestRunFrom(qid: Observation.Id, step: StepId)
      extends Action
  final case class RunFromComplete(id: Observation.Id, step: StepId)
      extends Action
  final case class RunFromFailed(id: Observation.Id, step: StepId)
      extends Action

  final case class  RunStarted(s:                Observation.Id) extends Action
  final case class  RunPaused(s:                 Observation.Id) extends Action
  final case class  RunCancelPaused(s:           Observation.Id) extends Action
  final case class  RunSync(s:                   Observation.Id) extends Action
  final case class  RunStartFailed(s:            Observation.Id) extends Action
  final case class  RunPauseFailed(s:            Observation.Id) extends Action
  final case class  RunCancelPauseFailed(s:      Observation.Id) extends Action
  final case class  RunSyncFailed(s:             Observation.Id) extends Action
  final case class  RunStop(s:                   Observation.Id) extends Action
  final case class  RunGracefulStop(s:           Observation.Id) extends Action
  final case class  RunStopCompleted(s:          Observation.Id) extends Action
  final case class  RunStopFailed(s:             Observation.Id) extends Action
  final case class  RunGracefulStopFailed(s:     Observation.Id) extends Action
  final case class  RunAbort(s:                  Observation.Id) extends Action
  final case class  RunAbortFailed(s:            Observation.Id) extends Action
  final case class  RunObsPause(s:               Observation.Id) extends Action
  final case class  RunGracefulObsPause(s:       Observation.Id) extends Action
  final case class  RunObsResume(s:              Observation.Id) extends Action
  final case class  RunObsPauseFailed(s:         Observation.Id) extends Action
  final case class  RunGracefulObsPauseFailed(s: Observation.Id) extends Action
  final case class  RunObsResumeFailed(s:        Observation.Id) extends Action

  final case class  ClearRunOnError(s:           Observation.Id)  extends Action
  final case class  ClearOperations(s:           Observation.Id)  extends Action
  final case object ClearAllOperations                            extends Action
  final case class  ClearAllResourceOperations(s: Observation.Id) extends Action
  final case class  ClearAllResourceOperationsOnStepChange(s: Observation.Id, step: StepId) extends Action
  final case class  ClearResourceOperations(s: Observation.Id, r: Resource)
      extends Action

  // Queue actions
  final case class RequestAllSelectedSequences(qid: QueueId)                               extends Action
  final case class AllDayCalCompleted(qid:          QueueId)                               extends Action
  final case class AllDayCalFailed(qid:             QueueId)                               extends Action
  final case class RequestClearAllCal(qid:          QueueId)                               extends Action
  final case class ClearAllCalCompleted(qid:        QueueId)                               extends Action
  final case class ClearAllCalFailed(qid:           QueueId)                               extends Action
  final case class RequestRunCal(qid:               QueueId)                               extends Action
  final case class RunCalCompleted(qid:             QueueId)                               extends Action
  final case class RunCalFailed(qid:                QueueId)                               extends Action
  final case class RequestStopCal(qid:              QueueId)                               extends Action
  final case class StopCalCompleted(qid:            QueueId)                               extends Action
  final case class StopCalFailed(qid:               QueueId)                               extends Action
  final case class RequestRemoveSeqCal(qid:         QueueId, id: Observation.Id)           extends Action
  final case class RequestAddSeqCal(qid:            QueueId, id: Observation.Id)           extends Action
  final case class RemoveSeqCalCompleted(qid:       QueueId)                               extends Action
  final case class RemoveSeqCalFailed(qid:          QueueId, id: Observation.Id)           extends Action
  final case class RequestMoveCal(qid:              QueueId, id: Observation.Id, pos: Int) extends Action
  final case class MoveCalCompleted(qid:            QueueId)                               extends Action
  final case class MoveCalFailed(qid:               QueueId, id: Observation.Id)           extends Action
  final case class ClearLastQueueOp(qid:            QueueId)                               extends Action

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

  final case object FlipSoundOnOff extends Action

  final case class UpdateObserver(id: Observation.Id, name: Observer) extends Action
  final case class UpdateDefaultObserver(name: Observer) extends Action
  final case class UpdateCalTabObserver(name:  Observer) extends Action
  final case class UpdateOperator(name:        Operator) extends Action
  final case class UpdateImageQuality(iq:      ImageQuality) extends Action
  final case class UpdateCloudCover(cc:        CloudCover) extends Action
  final case class UpdateSkyBackground(sb:     SkyBackground) extends Action
  final case class UpdateWaterVapor(wv:        WaterVapor) extends Action

  final case class UpdateStepsConfigTableState(s: TableState[StepConfigTable.TableColumn])
      extends Action
  final case class UpdateSessionQueueTableState(s: TableState[SessionQueueTable.TableColumn])
      extends Action
  final case class UpdateStepTableState(id: Observation.Id,
                                        s:  TableState[StepsTable.TableColumn])
      extends Action
  final case class UpdateSelectedStep(id: Observation.Id, step: StepId)
      extends Action
  final case class UpdateSelectedStepForce(id: Observation.Id, step: StepId)
      extends Action
  final case class UpdateCalTableState(id: QueueId,
                                       s:  TableState[CalQueueTable.TableColumn])
      extends Action
  final case class LoadSequence(observer: Observer,
                                i:        Instrument,
                                id:       Observation.Id)
      extends Action
  final case class SequenceLoadFailed(id:       Observation.Id) extends Action
  final case class RequestFailedNotification(r: RequestFailed) extends Action
  case object CleanSequences extends Action

  final case class UpdateSessionFilter(f: SessionQueueFilter => SessionQueueFilter) extends Action

  // Used for UI debugging
  final case class MarkStepAsRunning(s: Observation.Id, step: Int) extends Action

  // This, together with the whole pprint library, is shaken off when linking
  // for production, since show is not called in that case (see LoggingProcessor).
  private object Printer {
    private val pprinter: PPrinter =
      PPrinter(defaultHeight = 200, colorApplyPrefix = fansi.Color.Blue)

    def apply(x: Any): String =
      pprinter(x, initialOffset = 4).toString
  }

  private val stepInfo: PartialFunction[Step, Product] = {
    case i: StandardStep => (i.id, i.status, i.configStatus)
    case i: NodAndShuffleStep => (i.id, i.status, i.configStatus, i.nsStatus, i.pendingObserveCmd)
  }

  implicit val show: Show[Action] = Show.show {
    case FlipBreakpointStep(oid, st) =>
      s"FlipBreakpointStep(${oid.format}, ${st.id})"
    case FlipSkipStep(oid, st) =>
      s"FlipSkipStep(${oid.format}, ${st.id})"
    case s @ ServerMessage(u @ SeqexecModelUpdate(view)) =>
      val someSteps = view.sessionQueue.map(
        s =>
          (s"id: ${s.id.format}",
           s"steps: ${s.steps.length}",
           s"state: ${s.status}",
           s.steps
            .filter(x => List(StepState.Pending, StepState.Running, StepState.Aborted).exists(_ === x.status))
            .slice(0, scala.math.min(s.steps.length, 20))
            .collect(stepInfo)
          )
        )
      val dayCalQueue = view.queues.values.map(x => s"${x.execState} ${x.queue}").mkString(",")
      s"${s.getClass.getSimpleName}(\n" +
        s"  ${Printer(u.getClass.getSimpleName)},\n" +
        s"  dayCal: ${Printer(dayCalQueue)},\n" +
        s"  loaded: ${Printer(view.loaded)},\n" +
        s"  ${Printer(someSteps)}\n)"
    case a                                              =>
      s"$a"
  }
}
