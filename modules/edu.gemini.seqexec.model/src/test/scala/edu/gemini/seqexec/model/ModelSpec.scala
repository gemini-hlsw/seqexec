// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import Model._
import cats.tests.CatsSuite
import cats.kernel.laws.discipline._

/**
  * Tests Model typeclasses
  */
final class ModelSpec extends CatsSuite {
  import SharedModelArbitraries._

  checkAll("Eq[UserDetails]", EqTests[UserDetails].eqv)
  checkAll("Eq[SystemName]", EqTests[SystemName].eqv)
  checkAll("Eq[StepConfig]", EqTests[StepConfig].eqv)
  checkAll("Order[Resource]", OrderTests[Resource].order)
  checkAll("Eq[Instrument]", EqTests[Instrument].eqv)
  checkAll("Eq[Operator]", EqTests[Operator].eqv)
  checkAll("Eq[Observer]", EqTests[Observer].eqv)
  checkAll("Eq[SequenceId]", EqTests[SequenceId].eqv)
  checkAll("Eq[StepState]", EqTests[StepState].eqv)
  checkAll("Eq[ActionStatus]", EqTests[ActionStatus].eqv)
  checkAll("Eq[Step]", EqTests[Step].eqv)
  checkAll("Eq[StandardStep]", EqTests[StandardStep].eqv)
  checkAll("Eq[SequenceState]", EqTests[SequenceState].eqv)
  checkAll("Eq[ActionType]", EqTests[ActionType].eqv)
  checkAll("Eq[SequenceView]", EqTests[SequenceView].eqv)
}
