// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import events._

/**
 * Tests Event typeclasses
 */
final class SeqexecEventSpec extends CatsSuite with SequenceEventsArbitraries {
  checkAll("Eq[ConnectionOpenEvent]", EqTests[ConnectionOpenEvent].eqv)
  checkAll("Eq[SequenceStart]", EqTests[SequenceStart].eqv)
  checkAll("Eq[StepExecuted]", EqTests[StepExecuted].eqv)
  checkAll("Eq[FileIdStepExecuted]", EqTests[FileIdStepExecuted].eqv)
  checkAll("Eq[SequenceCompleted]", EqTests[SequenceCompleted].eqv)
  checkAll("Eq[SequenceLoaded]", EqTests[SequenceLoaded].eqv)
  checkAll("Eq[SequenceUnloaded]", EqTests[SequenceUnloaded].eqv)
  checkAll("Eq[StepBreakpointChanged]", EqTests[StepBreakpointChanged].eqv)
  checkAll("Eq[OperatorUpdated]", EqTests[OperatorUpdated].eqv)
  checkAll("Eq[ObserverUpdated]", EqTests[ObserverUpdated].eqv)
  checkAll("Eq[ConditionsUpdated]", EqTests[ConditionsUpdated].eqv)
  checkAll("Eq[StepSkipMarkChanged]", EqTests[StepSkipMarkChanged].eqv)
  checkAll("Eq[SequencePauseRequested]", EqTests[SequencePauseRequested].eqv)
  checkAll("Eq[SequencePauseCanceled]", EqTests[SequencePauseCanceled].eqv)
  checkAll("Eq[SequenceRefreshed]", EqTests[SequenceRefreshed].eqv)
  checkAll("Eq[ActionStopRequested]", EqTests[ActionStopRequested].eqv)
  checkAll("Eq[SequenceUpdated]", EqTests[SequenceUpdated].eqv)
  checkAll("Eq[SequencePaused]", EqTests[SequencePaused].eqv)
  checkAll("Eq[ExposurePaused]", EqTests[ExposurePaused].eqv)
  checkAll("Eq[SequenceError]", EqTests[SequenceError].eqv)
  checkAll("Eq[UserNotification]", EqTests[UserNotification].eqv)
  checkAll("Eq[UserPromptNotification]", EqTests[UserPromptNotification].eqv)
  checkAll("Eq[ObservationProgressEvent]", EqTests[ObservationProgressEvent].eqv)
  checkAll("Eq[SequenceStopped]", EqTests[SequenceStopped].eqv)
  checkAll("Eq[SequenceAborted]", EqTests[SequenceAborted].eqv)
  checkAll("Eq[GuideConfigUpdate]", EqTests[GuideConfigUpdate].eqv)
  checkAll("Order[ServerLogMessage]", OrderTests[ServerLogMessage].order)
}
