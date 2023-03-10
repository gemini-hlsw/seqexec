// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.effect.{ Async, IO, Ref }
import cats.syntax.all._
import edu.gemini.seqexec.server.tcs.{ BinaryOnOff, BinaryYesNo }
import lucuma.core.enums.LightSinkName
import munit.CatsEffectSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import seqexec.model.{ M1GuideConfig, M2GuideConfig, TelescopeGuideConfig }
import seqexec.model.`enum`.{ ComaOption, Instrument, M1Source, MountGuideOption, TipTiltSource }
import seqexec.server.InstrumentGuide
import seqexec.server.altair.AltairController
import seqexec.server.gems.Gems
import seqexec.server.gems.Gems._
import seqexec.server.gems.GemsController._
import seqexec.server.tcs.TcsController.{
  AGConfig,
  AoGuidersConfig,
  AoTcsConfig,
  GuiderConfig,
  GuiderSensorOff,
  GuiderSensorOn,
  InstrumentOffset,
  LightPath,
  LightSource,
  NodChopTrackingConfig,
  OIConfig,
  OffsetP,
  OffsetQ,
  P1Config,
  ProbeTrackingConfig,
  Subsystem,
  TelescopeConfig
}
import seqexec.server.tcs.TcsSouthController._
import seqexec.server.tcs.TestTcsEpics.{ ProbeGuideConfigVals, TestTcsEvent }
import shapeless.tag
import squants.Time
import squants.space.Length
import squants.space.LengthConversions.LengthConversions
import squants.space.AngleConversions.AngleConversions

class TcsSouthControllerEpicsAoSpec extends CatsEffectSuite {
  import TcsSouthControllerEpicsAoSpec._

  private implicit def unsafeLogger: Logger[IO] = NoOpLogger.impl[IO]

  private val baseStateWithGeMS = TestTcsEpics.defaultState.copy(
    absorbTipTilt = 1,
    m1GuideSource = "GAOS",
    m1Guide = BinaryOnOff.On,
    m2GuideState = BinaryOnOff.On,
    m2aoGuide = "ON",
    cwfs1Follow = true,
    cwfs2Follow = true,
    cwfs3Follow = true,
    sfName = "ao2gsaoi1",
    g1MapName = GemsSource.Cwfs1.some,
    g2MapName = GemsSource.Cwfs2.some,
    g3MapName = GemsSource.Cwfs3.some,
    g1GuideConfig = ProbeGuideConfigVals(1, 0, 0, 1),
    g2GuideConfig = ProbeGuideConfigVals(1, 0, 0, 1),
    g3GuideConfig = ProbeGuideConfigVals(1, 0, 0, 1),
    cwfs1Parked = false,
    cwfs2Parked = false,
    cwfs3Parked = false,
    odgw1Parked = true,
    odgw2Parked = true,
    odgw3Parked = true,
    odgw4Parked = true,
    gsaoiPort = 1,
    comaCorrect = "On",
    useAo = BinaryYesNo.Yes
  )

  private val baseStateWithGeMSPlusP1Guiding = baseStateWithGeMS.copy(
    p1FollowS = "On",
    p1Parked = false,
    pwfs1On = BinaryYesNo.Yes,
    pwfs1ProbeGuideConfig = ProbeGuideConfigVals(1, 0, 0, 1)
  )

  private val baseConfig: TcsSouthAoConfig = AoTcsConfig[GemsGuiders, GemsConfig](
    TelescopeGuideConfig(MountGuideOption.MountGuideOff,
                         M1GuideConfig.M1GuideOff,
                         M2GuideConfig.M2GuideOff
    ),
    TelescopeConfig(None, None),
    AoGuidersConfig(
      tag[P1Config](
        GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
      ),
      GemsGuiders(
        tag[CWFS1Config](
          GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
        ),
        tag[CWFS2Config](
          GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
        ),
        tag[CWFS3Config](
          GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
        ),
        tag[ODGW1Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
        tag[ODGW2Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
        tag[ODGW3Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
        tag[ODGW4Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff))
      ),
      tag[OIConfig](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff))
    ),
    AGConfig(LightPath(LightSource.AO, LightSinkName.Gsaoi), None),
    GemsOn(
      Cwfs1Usage.Use,
      Cwfs2Usage.Use,
      Cwfs3Usage.Use,
      Odgw1Usage.DontUse,
      Odgw2Usage.DontUse,
      Odgw3Usage.DontUse,
      Odgw4Usage.DontUse,
      P1Usage.DontUse,
      OIUsage.DontUse
    ),
    DummyInstrument(Instrument.Gsaoi, 1.millimeters.some)
  )

  private def buildGems(gemsConfig: GemsConfig, pr: Gaos.PauseResume[IO]): Gems[IO] = new Gems[IO] {
    override val cfg: GemsConfig = gemsConfig

    override def pauseResume(
      pauseReasons:  Gaos.PauseConditionSet,
      resumeReasons: Gaos.ResumeConditionSet
    ): IO[Gaos.PauseResume[IO]] = pr.pure[IO]

    override val stateGetter: GemsWfsState[IO] = Gems.GemsWfsState[IO](
      Cwfs1DetectorState.Off.pure[IO],
      Cwfs2DetectorState.Off.pure[IO],
      Cwfs3DetectorState.Off.pure[IO],
      Odgw1DetectorState.Off.pure[IO],
      Odgw2DetectorState.Off.pure[IO],
      Odgw3DetectorState.Off.pure[IO],
      Odgw4DetectorState.Off.pure[IO]
    )

    override def observe(
      config:  Either[AltairController.AltairConfig, GemsConfig],
      expTime: Time
    ): IO[Unit] = IO.unit

    override def endObserve(config: Either[AltairController.AltairConfig, GemsConfig]): IO[Unit] =
      IO.unit
  }

  val parkCommands = List(
    TestTcsEvent.Cwfs1ParkCmd,
    TestTcsEvent.Cwfs2ParkCmd,
    TestTcsEvent.Cwfs3ParkCmd,
    TestTcsEvent.Odgw1ParkCmd,
    TestTcsEvent.Odgw2ParkCmd,
    TestTcsEvent.Odgw3ParkCmd,
    TestTcsEvent.Odgw4ParkCmd
  )

  test("Don't touch guiding if configuration does not change") {

    val dumbEpics = buildTcsEpics[IO](baseStateWithGeMSPlusP1Guiding)

    val config: TcsSouthAoConfig = baseConfig.copy(
      tc = baseConfig.tc.copy(offsetA =
        InstrumentOffset(tag[OffsetP](0.arcseconds), tag[OffsetQ](0.arcseconds)).some
      ),
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.GAOS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.GAOS))
      ),
      gds = baseConfig.gds.copy(
        pwfs1 = tag[P1Config](
          GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
        )
      )
    )

    val gemsConfig = GemsOn(
      Cwfs1Usage.Use,
      Cwfs2Usage.Use,
      Cwfs3Usage.Use,
      Odgw1Usage.DontUse,
      Odgw2Usage.DontUse,
      Odgw3Usage.DontUse,
      Odgw4Usage.DontUse,
      P1Usage.Use,
      OIUsage.DontUse
    )

    val gems = buildGems(gemsConfig, Gaos.PauseResume[IO](none, none))

    for {
      d <- dumbEpics
      c  = TcsSouthControllerEpicsAo(d)
      _ <- c.applyAoConfig(TcsController.Subsystem.allButGaosNorOi.add(Subsystem.Gaos),
                           gems,
                           gemsConfig,
                           config
           )
      r <- d.outputF
    } yield assert(r.isEmpty)

  }

  private val guideOffEvents = List(
    TestTcsEvent.M1GuideCmd("off"),
    TestTcsEvent.M2GuideCmd("off"),
    TestTcsEvent.M2GuideConfigCmd("", "", "on"),
    TestTcsEvent.MountGuideCmd("", "off")
  )

  private val guideOnEvents = List(
    TestTcsEvent.M1GuideCmd("on"),
    TestTcsEvent.M2GuideCmd("on"),
    TestTcsEvent.MountGuideCmd("", "on")
  )

  test("Pause and resume guiding for offsets") {

    val dumbEpics = buildTcsEpics[IO](baseStateWithGeMSPlusP1Guiding)

    val config: TcsSouthAoConfig = baseConfig.copy(
      tc = baseConfig.tc.copy(offsetA =
        InstrumentOffset(tag[OffsetP](10.arcseconds), tag[OffsetQ](-5.arcseconds)).some
      ),
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.GAOS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.GAOS))
      ),
      gds = baseConfig.gds.copy(
        pwfs1 = tag[P1Config](
          GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
        )
      )
    )

    val gemsConfig = GemsOn(
      Cwfs1Usage.Use,
      Cwfs2Usage.Use,
      Cwfs3Usage.Use,
      Odgw1Usage.DontUse,
      Odgw2Usage.DontUse,
      Odgw3Usage.DontUse,
      Odgw4Usage.DontUse,
      P1Usage.Use,
      OIUsage.DontUse
    )

    val gems = buildGems(gemsConfig, Gaos.PauseResume[IO](IO.unit.some, IO.unit.some))

    for {
      d <- dumbEpics
      c  = TcsSouthControllerEpicsAo(d)
      _ <- c.applyAoConfig(TcsController.Subsystem.allButGaosNorOi.add(Subsystem.Gaos),
                           gems,
                           gemsConfig,
                           config
           )
      r <- d.outputF
    } yield {
      val (head, tail) = r.span {
        case TestTcsEvent.OffsetACmd(_, _) => false
        case _                             => true
      }

      assert(guideOffEvents.forall(head.contains))
      assert(guideOnEvents.forall(tail.contains))
      assert(!parkCommands.exists(r.contains))
    }

  }

  test("Pause guiding for a sky") {

    val dumbEpics = buildTcsEpics[IO](baseStateWithGeMSPlusP1Guiding)

    val config: TcsSouthAoConfig = baseConfig.copy(
      tc = baseConfig.tc.copy(offsetA =
        InstrumentOffset(tag[OffsetP](30.arcseconds), tag[OffsetQ](30.arcseconds)).some
      ),
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.GAOS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.GAOS))
      ),
      gds = baseConfig.gds.copy(
        aoguide = GemsGuiders(
          tag[CWFS1Config](
            GuiderConfig(ProbeTrackingConfig.Frozen, GuiderSensorOff)
          ),
          tag[CWFS2Config](
            GuiderConfig(ProbeTrackingConfig.Frozen, GuiderSensorOff)
          ),
          tag[CWFS3Config](
            GuiderConfig(ProbeTrackingConfig.Frozen, GuiderSensorOff)
          ),
          tag[ODGW1Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
          tag[ODGW2Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
          tag[ODGW3Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
          tag[ODGW4Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff))
        )
      )
    )

    val gemsConfig = GemsOn(
      Cwfs1Usage.Use,
      Cwfs2Usage.Use,
      Cwfs3Usage.Use,
      Odgw1Usage.DontUse,
      Odgw2Usage.DontUse,
      Odgw3Usage.DontUse,
      Odgw4Usage.DontUse,
      P1Usage.Use,
      OIUsage.DontUse
    )

    val gems = buildGems(gemsConfig, Gaos.PauseResume[IO](IO.unit.some, none))

    for {
      d <- dumbEpics
      c  = TcsSouthControllerEpicsAo(d)
      _ <- c.applyAoConfig(TcsController.Subsystem.allButGaosNorOi.add(Subsystem.Gaos),
                           gems,
                           gemsConfig,
                           config
           )
      r <- d.outputF
    } yield {
      val (head, tail) = r.span {
        case TestTcsEvent.OffsetACmd(_, _) => false
        case _                             => true
      }

      assert(guideOffEvents.forall(head.contains))
      assert(!guideOnEvents.exists(tail.contains))
      assert(!parkCommands.exists(r.contains))
    }

  }

  test("Resume guiding after a sky") {

    val dumbEpics = buildTcsEpics[IO](
      TestTcsEpics.defaultState.copy(
        p1Parked = false,
        pwfs1On = BinaryYesNo.Yes,
        sfName = "gsaoi",
        g1MapName = GemsSource.Cwfs1.some,
        g2MapName = GemsSource.Cwfs2.some,
        g3MapName = GemsSource.Cwfs3.some,
        gsaoiPort = 1,
        useAo = BinaryYesNo.Yes,
        xoffsetPoA1 = 10.0,
        yoffsetPoA1 = 10.0
      )
    )

    val config: TcsSouthAoConfig = baseConfig.copy(
      tc = baseConfig.tc.copy(offsetA =
        InstrumentOffset(tag[OffsetP](0.arcseconds), tag[OffsetQ](0.arcseconds)).some
      ),
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.GAOS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.GAOS))
      )
    )

    val gemsConfig = GemsOn(
      Cwfs1Usage.Use,
      Cwfs2Usage.Use,
      Cwfs3Usage.Use,
      Odgw1Usage.DontUse,
      Odgw2Usage.DontUse,
      Odgw3Usage.DontUse,
      Odgw4Usage.DontUse,
      P1Usage.Use,
      OIUsage.DontUse
    )

    val gems = buildGems(gemsConfig, Gaos.PauseResume[IO](IO.unit.some, none))

    for {
      d <- dumbEpics
      c  = TcsSouthControllerEpicsAo(d)
      _ <- c.applyAoConfig(TcsController.Subsystem.allButGaosNorOi.add(Subsystem.Gaos),
                           gems,
                           gemsConfig,
                           config
           )
      r <- d.outputF
    } yield {
      val (head, tail) = r.span {
        case TestTcsEvent.OffsetACmd(_, _) => false
        case _                             => true
      }

      assert(!guideOffEvents.forall(head.contains))
      assert(guideOnEvents.forall(tail.contains))
      assert(!parkCommands.exists(r.contains))
    }

  }

  test("Park unused GeMS guiders") {
    val dumbEpics = buildTcsEpics[IO](
      TestTcsEpics.defaultState.copy(
        p1Parked = false,
        pwfs1On = BinaryYesNo.Yes,
        p1FollowS = "On",
        pwfs1ProbeGuideConfig = ProbeGuideConfigVals(1, 0, 0, 1),
        sfName = "gsaoi",
        gsaoiPort = 1,
        useAo = BinaryYesNo.Yes,
        cwfs1Follow = true,
        cwfs2Follow = true,
        cwfs3Follow = true,
        cwfs1Parked = false,
        cwfs2Parked = false,
        cwfs3Parked = false,
        odgw1Parked = false,
        odgw2Parked = false,
        odgw3Parked = false,
        odgw4Parked = false
      )
    )

    val gemsConfig = GemsOff

    val config: TcsSouthAoConfig = baseConfig.copy(
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOff,
        M1GuideConfig.M1GuideOff,
        M2GuideConfig.M2GuideOff
      ),
      gds = baseConfig.gds.copy(
        aoguide = GemsGuiders(
          tag[CWFS1Config](
            GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
          ),
          tag[CWFS2Config](
            GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
          ),
          tag[CWFS3Config](
            GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
          ),
          tag[ODGW1Config](GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)),
          tag[ODGW2Config](GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)),
          tag[ODGW3Config](GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)),
          tag[ODGW4Config](GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff))
        )
      ),
      gaos = gemsConfig
    )

    val gems = buildGems(gemsConfig, Gaos.PauseResume[IO](IO.unit.some, none))

    for {
      d <- dumbEpics
      c  = TcsSouthControllerEpicsAo(d)
      _ <- c.applyAoConfig(TcsController.Subsystem.allButGaosNorOi.add(Subsystem.Gaos),
                           gems,
                           gemsConfig,
                           config
           )
      r <- d.outputF
    } yield assert(parkCommands.forall(r.contains))

  }

}

object TcsSouthControllerEpicsAoSpec {

  final case class DummyInstrument(id: Instrument, threshold: Option[Length])
      extends InstrumentGuide {
    override val instrument: Instrument = id

    override def oiOffsetGuideThreshold: Option[Length] = threshold
  }

  def buildTcsEpics[F[_]: Async](baseState: TestTcsEpics.State): F[TestTcsEpics[F]] =
    for {
      stR  <- Ref.of[F, TestTcsEpics.State](baseState)
      outR <- Ref.of[F, List[TestTcsEpics.TestTcsEvent]](List.empty)
    } yield TestTcsEpics[F](stR, outR)

}
