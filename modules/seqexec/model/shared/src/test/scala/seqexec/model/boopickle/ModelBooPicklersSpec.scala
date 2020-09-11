// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.boopickle

import cats.tests.CatsSuite
import gem.Observation
import gem.arb.ArbEnumerated.{arbEnumerated => oldArbEnumerated}
import lucuma.core.util.arb.ArbEnumerated._
import gem.arb.ArbObservation
import org.scalacheck.Arbitrary._
import seqexec.model.enum._
import seqexec.model._
import seqexec.model.events._
import seqexec.model.SeqexecModelArbitraries._
import seqexec.model.SequenceEventsArbitraries._
import seqexec.model.arb.all._
import squants.time.Time

/**
 * Tests Serialization/Deserialization using BooPickle
 */
final class BoopicklingSpec extends CatsSuite with ModelBooPicklers with ArbObservation {

  checkAll("Pickler[UserDetails]", PicklerTests[UserDetails].pickler)
  checkAll("Pickler[SequenceView]", PicklerTests[SequenceView].pickler)
  checkAll("Pickler[ConnectionOpenEvent]", PicklerTests[ConnectionOpenEvent].pickler)
  checkAll("Pickler[SequencesQueue[SequenceView]]",
           PicklerTests[SequencesQueue[SequenceView]].pickler
  )
  checkAll("Pickler[StepExecuted]", PicklerTests[StepExecuted].pickler)
  checkAll("Pickler[SequenceCompleted]", PicklerTests[SequenceCompleted].pickler)
  checkAll("Pickler[SequenceLoaded]", PicklerTests[SequenceLoaded].pickler)
  checkAll("Pickler[SequenceUnloaded]", PicklerTests[SequenceUnloaded].pickler)
  checkAll("Pickler[SequenceStopped]", PicklerTests[SequenceStopped].pickler)
  checkAll("Pickler[SequenceAborted]", PicklerTests[SequenceAborted].pickler)
  checkAll("Pickler[StepBreakpointChanged]", PicklerTests[StepBreakpointChanged].pickler)
  checkAll("Pickler[StepSkipMarkChanged]", PicklerTests[StepSkipMarkChanged].pickler)
  checkAll("Pickler[SequencePauseRequested]", PicklerTests[SequencePauseRequested].pickler)
  checkAll("Pickler[SequencePauseCanceled]", PicklerTests[SequencePauseCanceled].pickler)
  checkAll("Pickler[ActionStopRequested]", PicklerTests[ActionStopRequested].pickler)
  checkAll("Pickler[LoadSequenceUpdated]", PicklerTests[LoadSequenceUpdated].pickler)
  checkAll("Pickler[ClearLoadedSequencesUpdated.type]",
           PicklerTests[ClearLoadedSequencesUpdated].pickler
  )
  checkAll("Pickler[QueueManipulationOp]", PicklerTests[QueueManipulationOp].pickler)
  checkAll("Pickler[ObservationProgressEvent]", PicklerTests[ObservationProgressEvent].pickler)
  checkAll("Pickler[QueueUpdated]", PicklerTests[QueueUpdated].pickler)
  checkAll("Pickler[ObserveStage]", PicklerTests[ObserveStage].pickler)
  checkAll("Pickler[SequenceError]", PicklerTests[SequenceError].pickler)
  checkAll("Pickler[SequencePaused]", PicklerTests[SequencePaused].pickler)
  checkAll("Pickler[ExposurePaused]", PicklerTests[ExposurePaused].pickler)
  checkAll("Pickler[BatchCommandState]", PicklerTests[BatchCommandState].pickler)
  checkAll("Pickler[ExecutionQueueView]", PicklerTests[ExecutionQueueView].pickler)
  checkAll("Pickler[SequencesQueue[Observation.Id]]",
           PicklerTests[SequencesQueue[Observation.Id]].pickler
  )
  checkAll("Pickler[ImageQuality]", PicklerTests[ImageQuality].pickler)
  checkAll("Pickler[WaterVapor]", PicklerTests[WaterVapor].pickler)
  checkAll("Pickler[SkyBackground]", PicklerTests[SkyBackground].pickler)
  checkAll("Pickler[CloudCover]", PicklerTests[CloudCover].pickler)
  checkAll("Pickler[Conditions]", PicklerTests[Conditions].pickler)
  checkAll("Pickler[Notification]", PicklerTests[Notification].pickler)
  checkAll("Pickler[Notification.ResourceConflict]",
           PicklerTests[Notification.ResourceConflict].pickler
  )
  checkAll("Pickler[Notification.InstrumentInUse]",
           PicklerTests[Notification.InstrumentInUse].pickler
  )
  checkAll("Pickler[Notification.RequestFailed]", PicklerTests[Notification.RequestFailed].pickler)
  checkAll("Pickler[Notification.SubsystemBusy]", PicklerTests[Notification.SubsystemBusy].pickler)
  checkAll("Pickler[UserNotification]", PicklerTests[UserNotification].pickler)
  checkAll("Pickler[UserPrompt]", PicklerTests[UserPrompt].pickler)
  checkAll("Pickler[UserPrompt.TargetCheckOverride]",
           PicklerTests[UserPrompt.TargetCheckOverride].pickler
  )
  checkAll("Pickler[UserPromptNotification]", PicklerTests[UserPromptNotification].pickler)
  checkAll("Pickler[GuideConfigUpdate]", PicklerTests[GuideConfigUpdate].pickler)
  checkAll("Pickler[UserLoginRequest]", PicklerTests[UserLoginRequest].pickler)
  checkAll("Pickler[Instrument]", PicklerTests[Instrument].pickler)
  checkAll("Pickler[Resource]", PicklerTests[Resource].pickler)
  checkAll("Pickler[SystemName]", PicklerTests[SystemName].pickler)
  checkAll("Pickler[SequenceState]", PicklerTests[SequenceState].pickler)
  checkAll("Pickler[StepState]", PicklerTests[StepState].pickler)
  checkAll("Pickler[ActionStatus]", PicklerTests[ActionStatus].pickler)
  checkAll("Pickler[StandardStep]", PicklerTests[StandardStep].pickler)
  checkAll("Pickler[NodAndShuffleStage]", PicklerTests[NodAndShuffleStage].pickler)
  checkAll("Pickler[NSSubexposure]", PicklerTests[NSSubexposure].pickler)
  checkAll("Pickler[NSAction]", PicklerTests[NSAction].pickler)
  checkAll("Pickler[NodAndShuffleStatus]", PicklerTests[NodAndShuffleStatus].pickler)
  checkAll("Pickler[NodAndShuffleStep]", PicklerTests[NodAndShuffleStep].pickler)
  checkAll("Pickler[NSRunningState]", PicklerTests[NSRunningState].pickler)
  checkAll("Pickler[Step]", PicklerTests[Step].pickler)
  checkAll("Pickler[QueueId]", PicklerTests[QueueId].pickler)
  checkAll("Pickler[Time]", PicklerTests[Time].pickler)
  checkAll("Pickler[ObservationProgress]", PicklerTests[ObservationProgress].pickler)
  checkAll("Pickler[SingleActionEvent]", PicklerTests[SingleActionEvent].pickler)
  checkAll("Pickler[AlignAndCalibEvent]", PicklerTests[AlignAndCalibEvent].pickler)
  checkAll("Pickler[ComaOption]", PicklerTests[ComaOption].pickler)
  checkAll("Pickler[TipTiltSource]", PicklerTests[TipTiltSource].pickler)
  checkAll("Pickler[M1Source]", PicklerTests[M1Source].pickler)
  checkAll("Pickler[MountGuideOption]", PicklerTests[MountGuideOption].pickler)
  checkAll("Pickler[M1GuideConfig]", PicklerTests[M1GuideConfig].pickler)
  checkAll("Pickler[M2GuideConfig]", PicklerTests[M2GuideConfig].pickler)
  checkAll("Pickler[TelescopeGuideConfig]", PicklerTests[TelescopeGuideConfig].pickler)
}
