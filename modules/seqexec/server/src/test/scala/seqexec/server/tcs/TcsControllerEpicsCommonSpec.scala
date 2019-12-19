// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.effect.{IO, Sync, Timer}
import cats.effect.concurrent.Ref
import cats.implicits._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import edu.gemini.spModel.core.Wavelength
import gem.enum.LightSinkName.Gmos
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.noop.NoOpLogger
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should.Matchers._
import seqexec.model.{M1GuideConfig, M2GuideConfig, TelescopeGuideConfig}
import seqexec.model.enum.{ComaOption, Instrument, M1Source, MountGuideOption, TipTiltSource}
import seqexec.server.InstrumentGuide
import seqexec.server.tcs.TcsController.LightSource.Sky
import seqexec.server.tcs.TcsController.{AGConfig, BasicGuidersConfig, BasicTcsConfig, FocalPlaneOffset, GuiderConfig, GuiderSensorOff, GuiderSensorOn, HrwfsPickupPosition, InstrumentOffset, LightPath, NodChopTrackingConfig, OIConfig, OffsetP, OffsetQ, OffsetX, OffsetY, P1Config, P2Config, ProbeTrackingConfig, TelescopeConfig}
import shapeless.tag
import squants.space.{Arcseconds, Length, Microns, Millimeters}
import org.scalatest.flatspec.AnyFlatSpec
import seqexec.server.tcs.TestTcsEpics.{ProbeGuideConfigVals, TestTcsEvent}
import squants.space.AngleConversions._
import squants.space.LengthConversions._

import scala.concurrent.ExecutionContext

class TcsControllerEpicsCommonSpec extends AnyFlatSpec with PrivateMethodTester {

  import TcsControllerEpicsCommonSpec._

  private implicit def unsafeLogger: Logger[IO] = NoOpLogger.impl[IO]
  private implicit val ioTimer: Timer[IO] = IO.timer(ExecutionContext.global)

  private val baseCurrentStatus = BaseEpicsTcsConfig(
    Arcseconds(33.8),
    FocalPlaneOffset(tag[OffsetX](Millimeters(0.0)), tag[OffsetY](Millimeters(0.0))),
    Wavelength(Microns(400)),
    GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
    GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
    GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
    TelescopeGuideConfig(MountGuideOption.MountGuideOff, M1GuideConfig.M1GuideOff, M2GuideConfig.M2GuideOff),
    AoFold.Out,
    useAo = false,
    None,
    HrwfsPickupPosition.OUT,
    InstrumentPorts(
      flamingos2Port = 5,
      ghostPort = 0,
      gmosPort = 3,
      gnirsPort = 0,
      gpiPort = 0,
      gsaoiPort = 1,
      nifsPort = 0,
      niriPort = 0
    )
  )

  private val baseConfig = BasicTcsConfig(
    TelescopeGuideConfig(MountGuideOption.MountGuideOff, M1GuideConfig.M1GuideOff, M2GuideConfig.M2GuideOff),
    TelescopeConfig(None, None),
    BasicGuidersConfig(
      tag[P1Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
      tag[P2Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
      tag[OIConfig](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff))
    ),
    AGConfig(LightPath(Sky, Gmos), None),
    DummyInstrument(Instrument.GmosS, (1.millimeters).some)
  )

  private val mustPauseWhileOffsetting = PrivateMethod[Boolean]('mustPauseWhileOffsetting)

  "TcsControllerEpicsCommon" should "not pause guiding if it is not necessary" in {
    //No offset
    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      baseConfig
    ) shouldBe false

    //Offset, but no guider in use
    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
        InstrumentOffset(
          tag[OffsetP](pwfs1OffsetThreshold * 2 * FOCAL_PLANE_SCALE),
          tag[OffsetQ](Arcseconds(0.0))
        ).some
      )(baseConfig)
    ) shouldBe false
  }

  it should "decide if it can keep PWFS1 guiding active when applying an offset" in {
    //Big offset with PWFS1 in use
    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](pwfs1OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS1))
          ) >>>
          (BasicTcsConfig.gds ^|-> BasicGuidersConfig.pwfs1).set(
            tag[P1Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        ) (baseConfig)
    ) shouldBe true

    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](pwfs1OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideConfig.M1GuideOn(M1Source.PWFS1)
          ) >>>
          (BasicTcsConfig.gds ^|-> BasicGuidersConfig.pwfs1).set(
            tag[P1Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        ) (baseConfig)
    ) shouldBe true

    //Small offset with PWFS1 in use
    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](pwfs1OffsetThreshold / 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS1))
          ) >>>
          (BasicTcsConfig.gds ^|-> BasicGuidersConfig.pwfs1).set(
            tag[P1Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        ) (baseConfig)
    ) shouldBe false
  }

  it should "decide if it can keep PWFS2 guiding active when applying an offset" in {
    //Big offset with PWFS2 in use
    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](pwfs2OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS2))
          ) >>>
          (BasicTcsConfig.gds ^|-> BasicGuidersConfig.pwfs2).set(
            tag[P2Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        )(baseConfig)
    ) shouldBe true

    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](pwfs2OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideConfig.M1GuideOn(M1Source.PWFS2)
          ) >>>
          (BasicTcsConfig.gds ^|-> BasicGuidersConfig.pwfs2).set(
            tag[P2Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        )(baseConfig)
    ) shouldBe true

    //Small offset with PWFS2 in use
    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](pwfs2OffsetThreshold / 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS2))
          ) >>>
          (BasicTcsConfig.gds ^|-> BasicGuidersConfig.pwfs2).set(
            tag[P2Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        )(baseConfig)
    ) shouldBe false

  }

  it should "decide if it can keep OIWFS guiding active when applying an offset" in {
    val threshold = Millimeters(1.0)

    //Big offset with OIWFS in use
    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](threshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.OIWFS))
          ) >>>
          (BasicTcsConfig.gds ^|-> BasicGuidersConfig.oiwfs).set(
            tag[OIConfig](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          ) >>>
          BasicTcsConfig.inst.set(DummyInstrument(Instrument.GmosS, threshold.some))
        )(baseConfig)
    ) shouldBe true

    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](threshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideConfig.M1GuideOn(M1Source.OIWFS)
          ) >>>
          (BasicTcsConfig.gds ^|-> BasicGuidersConfig.oiwfs).set(
            tag[OIConfig](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          ) >>>
          BasicTcsConfig.inst.set(DummyInstrument(Instrument.GmosS, threshold.some))
        )(baseConfig)
    ) shouldBe true

    //Small offset with OIWFS in use
    TcsControllerEpicsCommon invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](threshold / 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.OIWFS))
          ) >>>
          (BasicTcsConfig.gds ^|-> BasicGuidersConfig.oiwfs).set(
            tag[OIConfig](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          ) >>>
          BasicTcsConfig.inst.set(DummyInstrument(Instrument.GmosS, threshold.some))
        )(baseConfig)
    ) shouldBe false
  }

  private val baseStateWithP1Guiding = TestTcsEpics.defaultState.copy(
    absorbTipTilt = 1,
    m1GuideSource = "PWFS1",
    m1Guide = BinaryOnOff.On,
    m2GuideState = BinaryOnOff.On,
    m2p1Guide = "ON",
    p1FollowS = "On",
    p1Parked = false,
    pwfs1On = BinaryYesNo.Yes,
    sfName = "gmos3",
    pwfs1ProbeGuideConfig = ProbeGuideConfigVals(1, 0, 0, 1),
    gmosPort = 3,
    comaCorrect = "On"
  )

  "TcsControllerEpicsCommon" should "not open PWFS1 loops if configuration does not change" in {
    val dumbEpics = buildTcsController[IO](baseStateWithP1Guiding)

    val config = baseConfig.copy(
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.PWFS1),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.PWFS1))
      ),
      gds = baseConfig.gds.copy(
        pwfs1 = tag[P1Config](GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn))
      )
    )

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaosNorOi, config)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    assert(result.isEmpty)

  }

  private val guideOffEvents = List(
    TestTcsEvent.M1GuideCmd("off"),
    TestTcsEvent.M2GuideCmd("off"),
    TestTcsEvent.M2GuideConfigCmd("", "", "on"),
    TestTcsEvent.MountGuideCmd("","off"),
  )

  private val guideOnEvents = List(
    TestTcsEvent.M1GuideCmd("on"),
    TestTcsEvent.M2GuideCmd("on"),
    TestTcsEvent.MountGuideCmd("","on"),
  )

  it should "open PWFS1 loops for an unguided configuration" in {
    val dumbEpics = buildTcsController[IO](baseStateWithP1Guiding)

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaosNorOi, baseConfig)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    (List(
      TestTcsEvent.Pwfs1StopObserveCmd,
      TestTcsEvent.Pwfs1ProbeFollowCmd("Off"),
      TestTcsEvent.Pwfs1ProbeGuideConfig("Off", "Off", "Off", "Off")
    ) ++ guideOffEvents).map{ result should contain (_) }

  }

  it should "open and close PWFS1 loops for a big enough offset" in {
    val dumbEpics = buildTcsController[IO](baseStateWithP1Guiding)

    val config = baseConfig.copy(
      tc = baseConfig.tc.copy(offsetA = InstrumentOffset(tag[OffsetP](10.arcseconds), tag[OffsetQ](0.arcseconds)).some),
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.PWFS1),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.PWFS1))
      ),
      gds = baseConfig.gds.copy(
        pwfs1 = tag[P1Config](GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn))
      )
    )

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaosNorOi, config)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    val (head, tail) = result.span{
      case TestTcsEvent.OffsetACmd(_, _) => false
      case _                             => true
    }

    guideOffEvents.map{ head should contain (_) }

    List(
      TestTcsEvent.Pwfs1StopObserveCmd,
      TestTcsEvent.Pwfs1ProbeFollowCmd("Off"),
      TestTcsEvent.Pwfs1ProbeGuideConfig("Off", "Off", "Off", "Off")
    ).map{ head should not contain(_) }

    guideOnEvents.map{ tail should contain (_) }

  }

  it should "close PWFS1 loops for a guided configuration" in {
    // Current Tcs state with PWFS1 guiding, but off
    val dumbEpics = buildTcsController[IO](TestTcsEpics.defaultState.copy(
        m1GuideSource = "PWFS1",
        m2p1Guide = "ON",
        p1Parked = false,
        sfName = "gmos3",
        gmosPort = 3,
      ))

    val config = baseConfig.copy(
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.PWFS1),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.PWFS1))
      ),
      gds = baseConfig.gds.copy(
        pwfs1 = tag[P1Config](GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn))
      )
    )

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaosNorOi, config)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    (List(
      TestTcsEvent.Pwfs1ObserveCmd,
      TestTcsEvent.Pwfs1ProbeFollowCmd("On"),
      TestTcsEvent.Pwfs1ProbeGuideConfig("On", "Off", "Off", "On")
    ) ++ guideOnEvents).map{ result should contain(_) }

  }

  val baseStateWithP2Guiding = TestTcsEpics.defaultState.copy(
    absorbTipTilt = 1,
    m1GuideSource = "PWFS2",
    m1Guide = BinaryOnOff.On,
    m2GuideState = BinaryOnOff.On,
    m2p2Guide = "ON",
    p2FollowS = "On",
    p2Parked = false,
    pwfs2On = BinaryYesNo.Yes,
    sfName = "gmos3",
    pwfs2ProbeGuideConfig = ProbeGuideConfigVals(1, 0, 0, 1),
    gmosPort = 3,
    comaCorrect = "On"
  )

  it should "not open PWFS2 loops if configuration does not change" in {

    val dumbEpics = buildTcsController[IO](baseStateWithP2Guiding)

    val config = baseConfig.copy(
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.PWFS2),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.PWFS2))
      ),
      gds = baseConfig.gds.copy(
        pwfs2 = tag[P2Config](GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn))
      )
    )

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaosNorOi, config)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    assert(result.isEmpty)

  }

  it should "open PWFS2 loops for an unguided configuration" in {
    val dumbEpics = buildTcsController[IO](baseStateWithP2Guiding)

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaosNorOi, baseConfig)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    (List(
      TestTcsEvent.Pwfs2StopObserveCmd,
      TestTcsEvent.Pwfs2ProbeFollowCmd("Off"),
      TestTcsEvent.Pwfs2ProbeGuideConfig("Off", "Off", "Off", "Off")
    ) ++ guideOffEvents).map{ result should contain (_) }

  }

  it should "open and close PWFS2 loops for a big enough offset" in {
    val dumbEpics = buildTcsController[IO](baseStateWithP2Guiding)

    val config = baseConfig.copy(
      tc = baseConfig.tc.copy(offsetA = InstrumentOffset(tag[OffsetP](10.arcseconds), tag[OffsetQ](0.arcseconds)).some),
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.PWFS2),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.PWFS2))
      ),
      gds = baseConfig.gds.copy(
        pwfs2 = tag[P2Config](GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn))
      )
    )

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaosNorOi, config)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    val (head, tail) = result.span{
      case TestTcsEvent.OffsetACmd(_, _) => false
      case _                             => true
    }

    guideOffEvents.map{ head should contain (_) }

    List(
      TestTcsEvent.Pwfs2StopObserveCmd,
      TestTcsEvent.Pwfs2ProbeFollowCmd("Off"),
      TestTcsEvent.Pwfs2ProbeGuideConfig("Off", "Off", "Off", "Off")
    ).map{ head should not contain(_) }

    guideOnEvents.map{ tail should contain (_) }

  }

  it should "close PWFS2 loops for a guided configuration" in {
    // Current Tcs state with PWFS2 guiding, but off
    val dumbEpics = buildTcsController[IO](TestTcsEpics.defaultState.copy(
        m1GuideSource = "PWFS2",
        m2p2Guide = "ON",
        p2Parked = false,
        sfName = "gmos3",
        gmosPort = 3,
      ))

    val config = baseConfig.copy(
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.PWFS2),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.PWFS2))
      ),
      gds = baseConfig.gds.copy(
        pwfs2 = tag[P2Config](GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn))
      )
    )

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaosNorOi, config)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    (List(
      TestTcsEvent.Pwfs2ObserveCmd,
      TestTcsEvent.Pwfs2ProbeFollowCmd("On"),
      TestTcsEvent.Pwfs2ProbeGuideConfig("On", "Off", "Off", "On")
    ) ++ guideOnEvents).map{ result should contain(_) }

  }

  val baseStateWithOIGuiding = TestTcsEpics.defaultState.copy(
    absorbTipTilt = 1,
    m1GuideSource = "OIWFS",
    m1Guide = BinaryOnOff.On,
    m2GuideState = BinaryOnOff.On,
    m2oiGuide = "ON",
    oiFollowS = "On",
    oiParked = false,
    oiwfsOn = BinaryYesNo.Yes,
    sfName = "gmos3",
    oiwfsProbeGuideConfig = ProbeGuideConfigVals(1, 0, 0, 1),
    gmosPort = 3,
    comaCorrect = "On"
  )

  it should "not open OIWFS loops if configuration does not change" in {

    val dumbEpics = buildTcsController[IO](baseStateWithOIGuiding)

    val config = baseConfig.copy(
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.OIWFS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.OIWFS))
      ),
      gds = baseConfig.gds.copy(
        oiwfs = tag[OIConfig](GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn))
      )
    )

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaos, config)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    assert(result.isEmpty)

  }

  it should "open OIWFS loops for an unguided configuration" in {
    val dumbEpics = buildTcsController[IO](baseStateWithOIGuiding)

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaos, baseConfig)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    (List(
      TestTcsEvent.OiwfsStopObserveCmd,
      TestTcsEvent.OiwfsProbeFollowCmd("Off"),
      TestTcsEvent.OiwfsProbeGuideConfig("Off", "Off", "Off", "Off")
    ) ++ guideOffEvents).map{ result should contain (_) }

  }

  it should "open and close OIWFS loops for a big enough offset" in {
    val dumbEpics = buildTcsController[IO](baseStateWithOIGuiding)

    val config = baseConfig.copy(
      tc = baseConfig.tc.copy(offsetA = InstrumentOffset(tag[OffsetP](10.arcseconds), tag[OffsetQ](0.arcseconds)).some),
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.OIWFS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.OIWFS))
      ),
      gds = baseConfig.gds.copy(
        oiwfs = tag[OIConfig](GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn))
      )
    )

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaos, config)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    val (head, tail) = result.span{
      case TestTcsEvent.OffsetACmd(_, _) => false
      case _                             => true
    }

    guideOffEvents.map{ head should contain (_) }

    List(
      TestTcsEvent.OiwfsStopObserveCmd,
      TestTcsEvent.OiwfsProbeFollowCmd("Off"),
      TestTcsEvent.OiwfsProbeGuideConfig("Off", "Off", "Off", "Off")
    ).map{ head should not contain(_) }

    guideOnEvents.map{ tail should contain (_) }

  }

  it should "close OIWFS loops for a guided configuration" in {
    // Current Tcs state with OIWFS guiding, but off
    val dumbEpics = buildTcsController[IO](TestTcsEpics.defaultState.copy(
        m1GuideSource = "OIWFS",
        m2oiGuide = "ON",
        oiParked = false,
        sfName = "gmos3",
        gmosPort = 3,
      ))

    val config = baseConfig.copy(
      gc = TelescopeGuideConfig(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.OIWFS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.OIWFS))
      ),
      gds = baseConfig.gds.copy(
        oiwfs = tag[OIConfig](GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn))
      )
    )

    val genOut: IO[List[TestTcsEpics.TestTcsEvent]] = for {
      d <- dumbEpics
      c = TcsControllerEpicsCommon(d)
      _ <- c.applyBasicConfig(TcsController.Subsystem.allButGaos, config)
      r <- d.outputF
    } yield r

    val result = genOut.unsafeRunSync

    (List(
      TestTcsEvent.OiwfsObserveCmd,
      TestTcsEvent.OiwfsProbeFollowCmd("On"),
      TestTcsEvent.OiwfsProbeGuideConfig("On", "Off", "Off", "On")
    ) ++ guideOnEvents).map{ result should contain(_) }

  }

}

object TcsControllerEpicsCommonSpec {
  final case class DummyInstrument(id: Instrument, threshold: Option[Length]) extends InstrumentGuide {
    override val instrument: Instrument = id

    override def oiOffsetGuideThreshold: Option[Length] = threshold
  }

  def buildTcsController[F[_]: Sync](baseState: TestTcsEpics.State): F[TestTcsEpics[F]] =
    for {
      stR  <- Ref.of[F, TestTcsEpics.State](baseState)
      outR <- Ref.of[F, List[TestTcsEpics.TestTcsEvent]](List.empty)
    }  yield new TestTcsEpics[F](stR, outR)
}
