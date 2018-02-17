// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import events.SeqexecModelUpdate
import events.SeqexecEvent._

/**
  * Tests Event typeclasses
  */
final class SeqexEventSpec extends CatsSuite {
  // import SharedModelArbitraries._
  import SequenceEventsArbitraries._

  checkAll("Eq[SeqexecModelUpdate]", EqTests[SeqexecModelUpdate].eqv)
  checkAll("Eq[ConnectionOpenEvent]", EqTests[ConnectionOpenEvent].eqv)
  checkAll("Eq[SequenceStart]", EqTests[SequenceStart].eqv)
  checkAll("Eq[StepExecuted]", EqTests[StepExecuted].eqv)
}
