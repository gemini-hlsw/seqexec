// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.implicits._
import edu.gemini.spModel.core.Wavelength
import gem.enum.LightSinkName.Gmos
import org.scalatest.{FlatSpec, PrivateMethodTester}
import org.scalatest.Matchers._
import seqexec.model.enum.Instrument
import seqexec.server.InstrumentGuide
import seqexec.server.tcs.TcsController.LightSource.Sky
import seqexec.server.tcs.TcsController.MountGuideOption.MountGuideOff
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsControllerEpics.{AoFold, EpicsTcsConfig, InstrumentPorts}
import shapeless.tag
import squants.space._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class TcsControllerEpicsSpec extends FlatSpec with PrivateMethodTester {
  import TcsControllerEpicsSpec._

  private val baseCurrentStatus = EpicsTcsConfig(
    Arcseconds(33.8),
    FocalPlaneOffset(tag[OffsetX](Millimeters(0.0)), tag[OffsetY](Millimeters(0.0))),
    Wavelength(Microns(400)),
    GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
    Left(GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
    GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
    TelescopeGuideConfig(MountGuideOff, M1GuideOff, M2GuideOff),
    AoFold.Out,
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

  private val baseConfig = TcsConfig(
    TelescopeGuideConfig(MountGuideOff, M1GuideOff, M2GuideOff),
    TelescopeConfig(None, None),
    GuidersConfig(
      tag[P1Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
      Left(tag[P2Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff))),
      tag[OIConfig](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff))
    ),
    AGConfig(LightPath(Sky, Gmos), None),
    None,
    DummyInstrument(None)
  )

  private val mustPauseWhileOffsetting = PrivateMethod[Boolean]('mustPauseWhileOffsetting)

  "TcsControllerEpics" should "not pause guiding if it is not necessary" in {
    //No offset
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      baseConfig
    ) shouldBe false

    //Offset, but no guider in use
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
        InstrumentOffset(
          tag[OffsetP](TcsControllerEpics.pwfs1OffsetThreshold * 2 * FOCAL_PLANE_SCALE),
          tag[OffsetQ](Arcseconds(0.0))
        ).some
      )(baseConfig)
    ) shouldBe false
  }

  it should "decide if it can keep PWFS1 guiding active when applying an offset" in {
    //Big offset with PWFS1 in use
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs1OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS1))
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.pwfs1).set(
            tag[P1Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        ) (baseConfig)
    ) shouldBe true

    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs1OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideOn(M1Source.PWFS1)
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.pwfs1).set(
            tag[P1Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        ) (baseConfig)
    ) shouldBe true

    //Small offset with PWFS1 in use
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs1OffsetThreshold / 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS1))
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.pwfs1).set(
            tag[P1Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        ) (baseConfig)
    ) shouldBe false
  }

  it should "decide if it can keep PWFS2 guiding active when applying an offset" in {
    //Big offset with PWFS2 in use
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs2OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS2))
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.pwfs2OrAowfs).set(Left(
            tag[P2Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            )))
          )
      )(baseConfig)
    ) shouldBe true

    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs2OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideOn(M1Source.PWFS2)
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.pwfs2OrAowfs).set(Left(
            tag[P2Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            )))
          )
      )(baseConfig)
    ) shouldBe true

    //Small offset with PWFS2 in use
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs2OffsetThreshold / 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS2))
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.pwfs2OrAowfs).set(Left(
            tag[P2Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            )))
          )
      )(baseConfig)
    ) shouldBe false

  }

  it should "decide if it can keep OIWFS guiding active when applying an offset" in {
    val threshold = Millimeters(1.0)

    //Big offset with OIWFS in use
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](threshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.OIWFS))
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.oiwfs).set(
            tag[OIConfig](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          ) >>>
          TcsConfig.inst.set(DummyInstrument(threshold.some))
      )(baseConfig)
    ) shouldBe true

    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](threshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideOn(M1Source.OIWFS)
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.oiwfs).set(
            tag[OIConfig](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          ) >>>
          TcsConfig.inst.set(DummyInstrument(threshold.some))
      )(baseConfig)
    ) shouldBe true

    //Small offset with OIWFS in use
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](threshold / 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.OIWFS))
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.oiwfs).set(
            tag[OIConfig](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          ) >>>
          TcsConfig.inst.set(DummyInstrument(threshold.some))
      )(baseConfig)
    ) shouldBe false
  }

  // The test uses only NIRI, although the threshold can be different for each instrument. The reason is that the
  // goal of the test is to check that it works in general, not to test for the specific threshold of every
  // instrument.
  it should "decide if it can keep Altair guiding active when applying an offset" in {
    val niriAoThreshold = Arcseconds(3.0)
    //Big offset with Altair in use
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](niriAoThreshold * 2.0),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.GAOS))
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.pwfs2OrAowfs).set(Right(
            tag[AoGuide](
              GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
            ))
          ) >>>
          TcsConfig.inst.set(DummyInstrument(none))
      )(baseConfig)
    ) shouldBe true

    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](niriAoThreshold * 2.0),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideOn(M1Source.GAOS)
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.pwfs2OrAowfs).set(Right(
            tag[AoGuide](
              GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
            ))
          ) >>>
          TcsConfig.inst.set(DummyInstrument(none))
      )(baseConfig)
    ) shouldBe true

    //Small offset with Altair in use
    TcsControllerEpics invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](niriAoThreshold / 2.0),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.GAOS))
          ) >>>
          (TcsConfig.gds ^|-> GuidersConfig.pwfs2OrAowfs).set(Right(
            tag[AoGuide](
              GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
            ))
          ) >>>
          TcsConfig.inst.set(DummyInstrument(none))
      )(baseConfig)
    ) shouldBe false

  }

}

object TcsControllerEpicsSpec {
  final case class DummyInstrument(threshold: Option[Length]) extends InstrumentGuide {
    override val instrument: Instrument = Instrument.Niri

    override def oiOffsetGuideThreshold: Option[Length] = threshold
  }
}