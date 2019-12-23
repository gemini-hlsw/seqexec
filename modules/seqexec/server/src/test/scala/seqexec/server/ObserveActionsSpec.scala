// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.tests.CatsSuite
import cats.effect._
import edu.gemini.spModel.config2.DefaultConfig
import edu.gemini.spModel.gemini.niri.InstNIRI._
import edu.gemini.spModel.gemini.niri.Niri.ReadMode
import edu.gemini.spModel.gemini.niri.Niri.WellDepth
import edu.gemini.spModel.gemini.niri.Niri.BuiltinROI
import edu.gemini.spModel.obscomp.InstConstants.DATA_LABEL_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.OBSERVE_KEY
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import gem.Observation
import gem.enum.Site
import io.chrisdavenport.log4cats.noop.NoOpLogger
import org.http4s.client.Client
import org.http4s.Uri
import org.http4s.Uri._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import shapeless.tag
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.niri.NiriController.DCConfig
import seqexec.server.niri.NiriController.NiriConfig
import seqexec.server.keywords.GdsClient
import seqexec.model.dhs._
import seqexec.model.enum._
import seqexec.model.config._
import seqexec.server.ConfigUtilOps._
import seqexec.server.niri._
import squants.Time
import squants.time.TimeConversions._

final class ObserveActionsSpec extends CatsSuite {
  private implicit def unsafeLogger = NoOpLogger.impl[IO]
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val noWaitTio: Timer[IO] = new Timer[IO] {
    override def clock: Clock[IO] = Clock.create[IO]
    override def sleep(duration: FiniteDuration): IO[Unit] =
      IO.unit
  }

  // This is a simulator for NIRI that never completes an obs
  // The engine should timeout and produce an error
  def noObserve: IO[NiriController[IO]] =
    InstrumentControllerSim[IO](s"NIRI").map { sim =>
      new NiriController[IO] {
        override def observe(
          fileId: ImageFileId,
          cfg:    DCConfig
        ): IO[ObserveCommandResult] =
          IO.never // obs neither fails nor completes

        override def applyConfig(config: NiriConfig): IO[Unit] =
          sim.applyConfig(config)

        override def stopObserve: IO[Unit] = sim.stopObserve

        override def abortObserve: IO[Unit] = sim.abortObserve

        override def endObserve: IO[Unit] = sim.endObserve

        override def observeProgress(total: Time): fs2.Stream[IO, Progress] =
          sim.observeCountdown(total, ElapsedTime(0.seconds))

        override def calcTotalExposureTime(cfg: DCConfig): IO[Time] = {
          val MinIntTime = 0.5.seconds

          (cfg.exposureTime + MinIntTime) * cfg.coadds.toDouble
        }.pure[IO]
      }
    }

  test("Observe Actions do timeout") {
    val inst   = Instrument.Niri
    val config = new DefaultConfig()
    config.putItem(OBSERVE_KEY / DATA_LABEL_PROP, "label")
    config.putItem(OBSERVE_KEY / EXPOSURE_TIME_PROP, 1.0)
    config.putItem(OBSERVE_KEY / COADDS_PROP, 1)
    config.putItem(INSTRUMENT_KEY / READ_MODE_PROP, ReadMode.DEFAULT)
    config.putItem(INSTRUMENT_KEY / WELL_DEPTH_PROP, WellDepth.DEFAULT)
    config.putItem(INSTRUMENT_KEY / BUILTIN_ROI_PROP, BuiltinROI.DEFAULT)
    println(config)
    val httpClient: Client[IO] = GdsClient.alwaysOkClient[IO]
    val control = SystemsControlConfiguration(
      altair   = ControlStrategy.Simulated,
      gems     = ControlStrategy.Simulated,
      dhs      = ControlStrategy.Simulated,
      f2       = ControlStrategy.Simulated,
      gcal     = ControlStrategy.Simulated,
      gmos     = ControlStrategy.Simulated,
      gnirs    = ControlStrategy.Simulated,
      gpi      = ControlStrategy.Simulated,
      gpiGds   = ControlStrategy.Simulated,
      ghost    = ControlStrategy.Simulated,
      ghostGds = ControlStrategy.Simulated,
      gsaoi    = ControlStrategy.Simulated,
      gws      = ControlStrategy.Simulated,
      nifs     = ControlStrategy.Simulated,
      niri     = ControlStrategy.Simulated,
      tcs      = ControlStrategy.Simulated
    )
    val conf = SeqexecEngineConfiguration(
      uri("http://127.0.0.1"),
      uri("http://127.0.0.1"),
      control,
      false,
      false,
      0,
      30.seconds,
      tag[GpiSettings][Uri](uri("http://127.0.0.1")),
      tag[GpiSettings][Uri](uri("http://127.0.0.1")),
      tag[GhostSettings][Uri](uri("http://127.0.0.1")),
      tag[GhostSettings][Uri](uri("http://127.0.0.1")),
      "",
      None,
      30.seconds
    )
    println(
      Systems
        .build(Site.GS, httpClient, conf, null)
        .use { s =>
          noObserve.flatMap { c =>
            val niri = Niri(c, s.dhs)
            val env = ObserveEnvironment[IO](
              s,
              CleanConfig(config),
              StepType.CelestialObject(inst),
              Observation.Id.unsafeFromString("GS-2019-Q-0-1"),
              niri,
              Nil,
              _ => Nil,
              null
            )
            ObserveActions.observePreamble[IO](toImageFileId("S001"), env)
          }
        }
        .attempt
        .unsafeRunSync
        .isLeft shouldBe true
    )
  }
}
