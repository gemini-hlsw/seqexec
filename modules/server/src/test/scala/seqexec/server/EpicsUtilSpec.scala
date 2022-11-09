// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import java.time.Duration
import java.util.concurrent.{ ScheduledExecutorService, ScheduledThreadPoolExecutor }

import cats.effect.IO
import edu.gemini.epics.acm.CaWindowStabilizer
import edu.gemini.epics.acm.test.DummyAttribute
import java.util.concurrent.TimeUnit.MILLISECONDS

import scala.concurrent.duration.FiniteDuration

class EpicsUtilSpec extends munit.CatsEffectSuite {

  val executor: ScheduledExecutorService = new ScheduledThreadPoolExecutor(2)

  test("EpicsUtil waitForValue should work with CaWindowStabilizer") {
    val attr: DummyAttribute[Integer]         = new DummyAttribute[Integer]("dummy", "foo")
    val filtered: CaWindowStabilizer[Integer] =
      new CaWindowStabilizer(attr, Duration.ofMillis(50), executor)
    attr.setValue(1)
    IO.delay(filtered.restart) *>
      EpicsUtil.waitForValueF[Integer, IO](filtered, 1, FiniteDuration(100, MILLISECONDS), "")
  }

}
