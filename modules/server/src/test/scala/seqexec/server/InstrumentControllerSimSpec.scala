// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import cats.effect.Ref
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import seqexec.model.dhs._
import seqexec.model.enum.ObserveCommandResult
import squants.time.TimeConversions._

import scala.concurrent.duration._

class InstrumentControllerSimSpec extends CatsEffectSuite {
  private implicit def unsafeLogger = NoOpLogger.impl[IO]

  val tick = FiniteDuration(250, MILLISECONDS)

  def simulator: InstrumentControllerSim[IO] =
    InstrumentControllerSim.unsafeWithTimes[IO]("sim",
                                                FiniteDuration(10, MILLISECONDS),
                                                FiniteDuration(5, MILLISECONDS),
                                                FiniteDuration(1, SECONDS)
    )

  test("simulation doesn't stack overflow".ignore) {
    val obsStateRef = Ref.unsafe[IO, InstrumentControllerSim.ObserveState](
      InstrumentControllerSim.ObserveState(false, false, false, Int.MaxValue)
    )
    val sim         = new InstrumentControllerSim.InstrumentControllerSimImpl[IO](
      "sim",
      false,
      FiniteDuration(10, MILLISECONDS),
      FiniteDuration(5, MILLISECONDS),
      FiniteDuration(1, SECONDS),
      obsStateRef
    )
    // We make a very long observation here to ensure we don't stack overflow
    sim.observeTic(None).map(assertEquals(_, ObserveCommandResult.Success))
  }
  test("normal observation") {
    simulator
      .observe(toImageFileId("S001"), 5.seconds)
      .map(assertEquals(_, ObserveCommandResult.Success))
  }
  test("pause observation") {
    val sim = simulator
    for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      r <- f.joinWithNever
    } yield assertEquals(r, ObserveCommandResult.Paused)
  }
  test("abort observation") {
    val sim = simulator
    for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.abortObserve
      r <- f.joinWithNever
    } yield assertEquals(r, ObserveCommandResult.Aborted)
  }
  test("stop observation") {
    val sim = simulator
    for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.stopObserve
      r <- f.joinWithNever
    } yield assertEquals(r, ObserveCommandResult.Stopped)
  }
  test("pause/stop pause observation") {
    val sim = simulator
    for {
      f <- sim.observe(toImageFileId("S001"), 900.milliseconds).start
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      r <- sim.stopPaused
      _ <- f.joinWithNever
    } yield assertEquals(r, ObserveCommandResult.Stopped)
  }
  test("pause/resume observation") {
    val sim = simulator
    for {
      f <- sim.observe(toImageFileId("S001"), 900.milliseconds).start
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      r <- sim.resumePaused
      _ <- f.joinWithNever
    } yield assertEquals(r, ObserveCommandResult.Success)
  }
  test("pause/stop observation") {
    val sim = simulator
    for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.stopObserve
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      r <- f.joinWithNever
    } yield assertEquals(r, ObserveCommandResult.Paused)
  }
  test("pause/abort paused observation") {
    val sim = simulator
    for {
      f <- sim.observe(toImageFileId("S001"), 900.milliseconds).start
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      r <- sim.abortPaused
      _ <- f.joinWithNever
    } yield assertEquals(r, ObserveCommandResult.Aborted)
  }
  test("pause/abort observation") {
    val sim = simulator
    for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      _ <- IO.sleep(tick) // give it enough time for at least one tick
      _ <- sim.abortObserve
      r <- f.joinWithNever
    } yield assertEquals(r, ObserveCommandResult.Paused)
  }
}
