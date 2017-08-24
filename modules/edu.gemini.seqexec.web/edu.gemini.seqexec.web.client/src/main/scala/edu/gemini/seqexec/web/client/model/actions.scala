// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.model


import diode.Action
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.Model.SeqexecEvent.ServerLogMessage
import org.scalajs.dom.WebSocket


// scalastyle:off
// Actions
final case class NavigateTo(page: Pages.SeqexecPages) extends Action
final case class NavigateSilentTo(page: Pages.SeqexecPages) extends Action
final case class SyncToPage(view: SequenceView) extends Action
final case class SyncToRunning(view: SequenceView) extends Action
final case class SyncPageToRemovedSequence(id: SequenceId) extends Action
final case class SyncPageToAddedSequence(i: Instrument, id: SequenceId) extends Action
final case class Initialize(site: SeqexecSite) extends Action

// Actions to close and/open the login box
case object OpenLoginBox extends Action
case object CloseLoginBox extends Action

final case class LoggedIn(u: UserDetails) extends Action
case object Logout extends Action

// Action to select a sequence for display
final case class SelectIdToDisplay(id: SequenceId) extends Action
final case class SelectInstrumentToDisplay(i: Instrument) extends Action

// Actions related to executing sequences
final case class RequestRun(s: SequenceId) extends Action
final case class RequestSync(s: SequenceId) extends Action
final case class RequestPause(s: SequenceId) extends Action
final case class RunStarted(s: SequenceId) extends Action
final case class RunPaused(s: SequenceId) extends Action
final case class RunSync(s: SequenceId) extends Action
final case class RunStartFailed(s: SequenceId) extends Action
final case class RunPauseFailed(s: SequenceId) extends Action
final case class RunSyncFailed(s: SequenceId) extends Action

final case class ShowStep(s: SequenceId, i: Int) extends Action
final case class UnShowStep(i: Instrument) extends Action
final case class RememberCompleted(s: SequenceView) extends Action

final case class AppendToLog(l: ServerLogMessage) extends Action

// Actions related to web sockets
final case class WSConnect(delay: Int) extends Action
case object Connecting extends Action
final case class Connected(ws: WebSocket, delay: Int) extends Action
final case class ConnectionClosed(delay: Int) extends Action
final case class ConnectionError(s: String) extends Action
final case class ServerMessage(e: SeqexecEvent) extends Action

// Temporal actions for UI prototyping
final case class FlipSkipStep(id: SequenceId, step: Step) extends Action
final case class FlipBreakpointStep(id: SequenceId, step: Step) extends Action
final case class UpdateObserver(id: SequenceId, name: String) extends Action
final case class UpdateOperator(name: String) extends Action
final case class UpdateImageQuality(iq: ImageQuality) extends Action
final case class UpdateCloudCover(cc: CloudCover) extends Action
final case class UpdateSkyBackground(sb: SkyBackground) extends Action
final case class UpdateWaterVapor(wv: WaterVapor) extends Action

// scalastyle:on
