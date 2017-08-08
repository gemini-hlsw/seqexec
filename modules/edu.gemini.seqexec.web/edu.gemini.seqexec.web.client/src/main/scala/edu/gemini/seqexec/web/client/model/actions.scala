package edu.gemini.seqexec.web.client.model


import diode.Action
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.model.Model._
import org.scalajs.dom.WebSocket


// scalastyle:off
// Actions
case class NavigateTo(page: Pages.SeqexecPages) extends Action
case class NavigateSilentTo(page: Pages.SeqexecPages) extends Action
case class SyncToPage(view: SequenceView) extends Action
case class Initialize(site: SeqexecSite) extends Action

// Actions to close and/open the login box
case object OpenLoginBox extends Action
case object CloseLoginBox extends Action

case class LoggedIn(u: UserDetails) extends Action
case object Logout extends Action

// Action to select a sequence for display
case class SelectToDisplay(s: SequenceView) extends Action
case class SelectIdToDisplay(id: SequenceId) extends Action
case class SelectInstrumentToDisplay(i: Instrument) extends Action

// Actions related to executing sequences
case class RequestRun(s: SequenceId) extends Action
case class RequestSync(s: SequenceId) extends Action
case class RequestPause(s: SequenceId) extends Action
case class RunStarted(s: SequenceId) extends Action
case class RunPaused(s: SequenceId) extends Action
case class RunSync(s: SequenceId) extends Action
case class RunStartFailed(s: SequenceId) extends Action
case class RunPauseFailed(s: SequenceId) extends Action
case class RunSyncFailed(s: SequenceId) extends Action

case class ShowStep(s: SequenceId, i: Int) extends Action
case class UnShowStep(i: Instrument) extends Action

case class AppendToLog(s: String) extends Action

// Actions related to web sockets
case class WSConnect(delay: Int) extends Action
case object Connecting extends Action
case class Connected(ws: WebSocket, delay: Int) extends Action
case class ConnectionClosed(delay: Int) extends Action
case class ConnectionError(s: String) extends Action
case class ServerMessage(e: SeqexecEvent) extends Action

// Temporal actions for UI prototyping
case class FlipSkipStep(id: SequenceId, step: Step) extends Action
case class FlipBreakpointStep(id: SequenceId, step: Step) extends Action
case class UpdateObserver(id: SequenceId, name: String) extends Action
case class UpdateOperator(name: String) extends Action
case class UpdateImageQuality(iq: ImageQuality) extends Action
case class UpdateCloudCover(cc: CloudCover) extends Action
case class UpdateSkyBackground(sb: SkyBackground) extends Action
case class UpdateWaterVapor(wv: WaterVapor) extends Action

// scalastyle:on
