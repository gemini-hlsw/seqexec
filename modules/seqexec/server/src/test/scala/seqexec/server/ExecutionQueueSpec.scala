// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.tests.CatsSuite
import monocle.law.discipline.LensTests
import SeqexecServerArbitraries._
import seqexec.model.SeqexecModelArbitraries._

final class ExecutionQueueSpec extends CatsSuite {
  checkAll("ExecutedQueue name lens", LensTests(ExecutionQueue.name))
  checkAll("ExecutedQueue command state lens",
           LensTests(ExecutionQueue.cmdState))
  checkAll("ExecutedQueue queue lens", LensTests(ExecutionQueue.queue))
}
