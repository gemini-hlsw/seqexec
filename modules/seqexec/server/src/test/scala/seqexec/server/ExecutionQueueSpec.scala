// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.tests.CatsSuite
import gem.arb.ArbObservation._
import monocle.law.discipline.LensTests
import seqexec.model.SeqexecModelArbitraries._

final class ExecutionQueueSpec extends CatsSuite with SeqexecServerArbitraries {
  checkAll("ExecutionQueue name lens", LensTests(ExecutionQueue.name))
  checkAll("ExecutionQueue command state lens",
           LensTests(ExecutionQueue.cmdState))
  checkAll("ExecutionQueue queue lens", LensTests(ExecutionQueue.queue))
}
