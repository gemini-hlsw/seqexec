// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import seqexec.model.enum._
import seqexec.model.SeqexecModelArbitraries._

/**
  * Tests Model typeclasses
  */
final class ModelSpec extends CatsSuite {

  checkAll("Eq[UserDetails]", EqTests[UserDetails].eqv)
  checkAll("Eq[SystemName]", EqTests[SystemName].eqv)
  checkAll("Eq[StepConfig]", EqTests[StepConfig].eqv)
  checkAll("Order[Resource]", OrderTests[Resource].order)
  checkAll("Eq[Instrument]", EqTests[Instrument].eqv)
  checkAll("Eq[Operator]", EqTests[Operator].eqv)
  checkAll("Eq[Observer]", EqTests[Observer].eqv)
  checkAll("Eq[StepState]", EqTests[StepState].eqv)
  checkAll("Eq[ActionStatus]", EqTests[ActionStatus].eqv)
  checkAll("Eq[Step]", EqTests[Step].eqv)
  checkAll("Eq[StandardStep]", EqTests[StandardStep].eqv)
  checkAll("Eq[SequenceState]", EqTests[SequenceState].eqv)
  checkAll("Eq[ActionType]", EqTests[ActionType].eqv)
  checkAll("Eq[SequenceMetadata]", EqTests[SequenceMetadata].eqv)
  checkAll("Eq[SequenceView]", EqTests[SequenceView].eqv)
  checkAll("Eq[SequencesQueue[SequenceView]]", EqTests[SequencesQueue[SequenceView]].eqv)
  checkAll("Eq[StepType]", EqTests[StepType].eqv)
  checkAll("Order[TelescopeOffset.P]", OrderTests[TelescopeOffset.P].order)
  checkAll("Order[TelescopeOffset.Q]", OrderTests[TelescopeOffset.Q].order)
  checkAll("Eq[TelescopeOffset]", EqTests[TelescopeOffset].eqv)
  checkAll("Eq[Guiding]", EqTests[Guiding].eqv)
  checkAll("Eq[FPUMode]", EqTests[FPUMode].eqv)
  checkAll("Eq[CloudCover]", EqTests[CloudCover].eqv)
  checkAll("Eq[WaterVapor]", EqTests[WaterVapor].eqv)
  checkAll("Eq[ImageQuality]", EqTests[ImageQuality].eqv)
  checkAll("Eq[SkyBackground]", EqTests[SkyBackground].eqv)
  checkAll("Eq[Conditions]", EqTests[Conditions].eqv)
  checkAll("Eq[ClientID]", EqTests[ClientID].eqv)
  checkAll("Eq[ServerLogLevel]", EqTests[ServerLogLevel].eqv)
}
