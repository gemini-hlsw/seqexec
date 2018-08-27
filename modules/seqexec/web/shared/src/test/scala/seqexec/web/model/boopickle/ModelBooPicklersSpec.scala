// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.boopickle

import seqexec.model.enum._
import seqexec.model._
import seqexec.model.events._
import cats.tests.CatsSuite
import _root_.boopickle.Default._
import org.scalacheck.Arbitrary._
import seqexec.model.SeqexecModelArbitraries._
import seqexec.model.SequenceEventsArbitraries._
import gem.Observation

/**
  * Tests Serialization/Deserialization using BooPickle
  */
@SuppressWarnings(
  Array(
    "org.wartremover.warts.ImplicitParameter",
    "org.wartremover.warts.Throw",
    "org.wartremover.warts.OptionPartial",
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Equals"
  ))
final class BoopicklingSpec extends CatsSuite with ModelBooPicklers {

  checkAll("Pickler[UserDetails]", PicklerTests[UserDetails].pickler)
  checkAll("Pickler[SequenceView]", PicklerTests[SequenceView].pickler)
  checkAll("Pickler[ConnectionOpenEvent]",
           PicklerTests[ConnectionOpenEvent].pickler)
  checkAll("Pickler[SequencesQueue[SequenceView]]",
           PicklerTests[SequencesQueue[SequenceView]].pickler)
  checkAll("Pickler[StepExecuted]", PicklerTests[StepExecuted].pickler)
  checkAll("Pickler[SequenceCompleted]",
           PicklerTests[SequenceCompleted].pickler)
  checkAll("Pickler[SequenceLoaded]", PicklerTests[SequenceLoaded].pickler)
  checkAll("Pickler[SequenceUnloaded]", PicklerTests[SequenceUnloaded].pickler)
  checkAll("Pickler[StepBreakpointChanged]",
           PicklerTests[StepBreakpointChanged].pickler)
  checkAll("Pickler[StepSkipMarkChanged]",
           PicklerTests[StepSkipMarkChanged].pickler)
  checkAll("Pickler[SequencePauseRequested]",
           PicklerTests[SequencePauseRequested].pickler)
  checkAll("Pickler[SequencePauseCanceled]",
           PicklerTests[SequencePauseCanceled].pickler)
  checkAll("Pickler[ActionStopRequested]",
           PicklerTests[ActionStopRequested].pickler)
  checkAll("Pickler[LoadSequenceUpdated]",
           PicklerTests[LoadSequenceUpdated].pickler)
  checkAll("Pickler[ClearLoadedSequencesUpdated.type]",
           PicklerTests[ClearLoadedSequencesUpdated].pickler)
  checkAll("Pickler[SequenceError]", PicklerTests[SequenceError].pickler)
  checkAll("Pickler[SequencePaused]", PicklerTests[SequencePaused].pickler)
  checkAll("Pickler[ExposurePaused]", PicklerTests[ExposurePaused].pickler)
  checkAll("Pickler[SequencesQueue[Observation.Id]]",
           PicklerTests[SequencesQueue[Observation.Id]].pickler)
  checkAll("Pickler[ImageQuality]", PicklerTests[ImageQuality].pickler)
  checkAll("Pickler[WaterVapor]", PicklerTests[WaterVapor].pickler)
  checkAll("Pickler[SkyBackground]", PicklerTests[SkyBackground].pickler)
  checkAll("Pickler[CloudCover]", PicklerTests[CloudCover].pickler)
  checkAll("Pickler[Conditions]", PicklerTests[Conditions].pickler)
}
