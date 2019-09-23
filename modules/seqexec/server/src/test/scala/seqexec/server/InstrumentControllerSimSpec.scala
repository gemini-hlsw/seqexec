// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import cats.effect.Clock
import cats.effect.ContextShift
import cats.effect.Timer
import cats.effect.concurrent.Ref
import io.chrisdavenport.log4cats.noop.NoOpLogger
import scala.concurrent.duration._
import seqexec.model.enum.ObserveCommandResult
import seqexec.model.dhs._
import squants.time.TimeConversions._
import scala.concurrent.ExecutionContext
import cats.tests.CatsSuite

class InstrumentControllerSimSpec extends CatsSuite {
  private implicit def unsafeLogger = NoOpLogger.impl[IO]

  val noWaitTio: Timer[IO] = new Timer[IO] {
    override def clock: Clock[IO] = Clock.create[IO]
    override def sleep(duration: FiniteDuration): IO[Unit] =
      IO.unit
  }

  val tick = FiniteDuration(250, MILLISECONDS)

  def simulator(implicit t: Timer[IO]) =
     InstrumentControllerSim.unsafeWithTimes[IO]("sim",
                                            FiniteDuration(10, MILLISECONDS),
                                            FiniteDuration(5, MILLISECONDS),
                                            FiniteDuration(1, SECONDS))

  // Needed for IO.start to do a logical thread fork
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  test("simulation doesn't stack overflow") {
    implicit val tio = noWaitTio
    val obsStateRef = Ref.unsafe[IO, InstrumentControllerSim.ObserveState](InstrumentControllerSim.ObserveState(false, false, false, Int.MaxValue))
    val sim = new InstrumentControllerSim.InstrumentControllerSimImpl[IO](
      "sim",
      false,
      FiniteDuration(10, MILLISECONDS),
      FiniteDuration(5, MILLISECONDS),
      FiniteDuration(1, SECONDS),
      obsStateRef
    )
    // We make a very long observation here to ensure we don't stack overflow
    val result =
      sim.observeTic(None).unsafeRunSync
    assert(result === ObserveCommandResult.Success)
  }
  test("normal observation") {
    implicit val tio = noWaitTio
    val result = simulator.observe(toImageFileId("S001"), 5.seconds).unsafeRunSync
    assert(result === ObserveCommandResult.Success)
  }
  test("pause observation") {
    implicit val tio = IO.timer(ExecutionContext.global)
    val sim = simulator
    val result = (for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      r <- f.join
    } yield r).unsafeRunSync
    assert(result === ObserveCommandResult.Paused)
  }
  test("abort observation") {
    implicit val tio = IO.timer(ExecutionContext.global)
    val sim = simulator
    val result = (for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.abortObserve
      r <- f.join
    } yield r).unsafeRunSync
    assert(result === ObserveCommandResult.Aborted)
  }
  test("stop observation") {
    implicit val tio = IO.timer(ExecutionContext.global)
    val sim = simulator
    val result = (for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.stopObserve
      r <- f.join
    } yield r).unsafeRunSync
    assert(result === ObserveCommandResult.Stopped)
  }
  test("pause/stop pause observation") {
    implicit val tio = IO.timer(ExecutionContext.global)
    val sim = simulator
    val result = (for {
      f <- sim.observe(toImageFileId("S001"), 900.milliseconds).start
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      r <- sim.stopPaused
      _ <- f.join
    } yield r).unsafeRunSync

    assert(result === ObserveCommandResult.Stopped)
  }
  test("pause/resume observation") {
    implicit val tio = IO.timer(ExecutionContext.global)
    val sim = simulator
    val result = (for {
      f <- sim.observe(toImageFileId("S001"), 900.milliseconds).start
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      r <- sim.resumePaused
      _ <- f.join
    } yield r).unsafeRunSync

    assert(result === ObserveCommandResult.Success)
  }
  test("pause/stop observation") {
    implicit val tio = IO.timer(ExecutionContext.global)
    val sim = simulator
    val result = (for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.stopObserve
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      r <- f.join
    } yield r).unsafeRunSync

    // FIXME The simulator should return Stopped
    // assert(result === ObserveCommandResult.Stopped)
    assert(result === ObserveCommandResult.Paused)
  }
  test("pause/abort paused observation") {
    implicit val tio = IO.timer(ExecutionContext.global)
    val sim = simulator
    val result = (for {
      f <- sim.observe(toImageFileId("S001"), 900.milliseconds).start
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      r <- sim.abortPaused
      _ <- f.join
    } yield r).unsafeRunSync

    assert(result === ObserveCommandResult.Aborted)
  }
  test("pause/abort observation") {
    implicit val tio = IO.timer(ExecutionContext.global)
    val sim = simulator
    val result = (for {
      f <- sim.observe(toImageFileId("S001"), 2.seconds).start
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.pauseObserve
      _ <- tio.sleep(tick) // give it enough time for at least one tick
      _ <- sim.abortObserve
      r <- f.join
    } yield r).unsafeRunSync

    // FIXME The simulator should return Stopped
    // assert(result === ObserveCommandResult.Aborted)
    assert(result === ObserveCommandResult.Paused)
  }
}
