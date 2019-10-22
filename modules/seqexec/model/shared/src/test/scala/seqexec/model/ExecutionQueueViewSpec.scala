// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.tests.CatsSuite
import gem.arb.ArbEnumerated._
import gem.arb.ArbObservation._
import monocle.law.discipline.LensTests
import seqexec.model.SeqexecModelArbitraries._

final class ExecutionQueueViewSpec extends CatsSuite {
  checkAll("ExecutionQueueView id lens", LensTests(ExecutionQueueView.id))
  checkAll("ExecutionQueueView name lens", LensTests(ExecutionQueueView.name))
  checkAll("ExecutionQueueView command state lens",
           LensTests(ExecutionQueueView.cmdState))
  checkAll("ExecutionQueueView execution state lens",
           LensTests(ExecutionQueueView.execState))
  checkAll("ExecutionQueueView queue lens", LensTests(ExecutionQueueView.queue))
}
