// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.implicits._
import edu.gemini.spModel.core.Wavelength
import gem.enum.LightSinkName.Gmos
import org.scalatest.{FlatSpec, PrivateMethodTester}
import org.scalatest.Matchers._
import seqexec.model.enum._
import seqexec.model.TelescopeGuideConfig
import seqexec.model.M1GuideConfig
import seqexec.model.M2GuideConfig
import seqexec.server.InstrumentGuide
import seqexec.server.tcs.TcsController.LightSource.Sky
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsControllerEpics.{AoFold, InstrumentPorts}
import seqexec.server.tcs.TcsSouthController.{GuidersConfig, TcsSouthConfig}
import seqexec.server.tcs.TcsSouthControllerEpics.EpicsTcsConfig
import shapeless.tag
import squants.space._

class TcsSouthControllerEpicsSpec extends FlatSpec with PrivateMethodTester {
  import TcsSouthControllerEpicsSpec._

  private val baseCurrentStatus = EpicsTcsConfig(
    Arcseconds(33.8),
    FocalPlaneOffset(tag[OffsetX](Millimeters(0.0)), tag[OffsetY](Millimeters(0.0))),
    Wavelength(Microns(400)),
    GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
    GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
    GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
    TelescopeGuideConfig(MountGuideOption.MountGuideOff, M1GuideConfig.M1GuideOff, M2GuideConfig.M2GuideOff),
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

  private val baseConfig = TcsSouthConfig(
    TelescopeGuideConfig(MountGuideOption.MountGuideOff, M1GuideConfig.M1GuideOff, M2GuideConfig.M2GuideOff),
    TelescopeConfig(None, None),
    GuidersConfig(
      tag[P1Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
      tag[P2Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
      tag[OIConfig](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff))
    ),
    AGConfig(LightPath(Sky, Gmos), None),
    None,
    DummyInstrument(None)
  )

  private val mustPauseWhileOffsetting = PrivateMethod[Boolean]('mustPauseWhileOffsetting)

  "TcsSouthControllerEpics" should "not pause guiding if it is not necessary" in {
    //No offset
    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      baseConfig
    ) shouldBe false

    //Offset, but no guider in use
    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
        InstrumentOffset(
          tag[OffsetP](TcsControllerEpics.pwfs1OffsetThreshold * 2 * FOCAL_PLANE_SCALE),
          tag[OffsetQ](Arcseconds(0.0))
        ).some
      )(baseConfig)
    ) shouldBe false
  }

  it should "decide if it can keep PWFS1 guiding active when applying an offset" in {
    //Big offset with PWFS1 in use
    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs1OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsSouthConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS1))
          ) >>>
          (TcsSouthConfig.gds ^|-> GuidersConfig.pwfs1).set(
            tag[P1Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        ) (baseConfig)
    ) shouldBe true

    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs1OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsSouthConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideConfig.M1GuideOn(M1Source.PWFS1)
          ) >>>
          (TcsSouthConfig.gds ^|-> GuidersConfig.pwfs1).set(
            tag[P1Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        ) (baseConfig)
    ) shouldBe true

    //Small offset with PWFS1 in use
    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs1OffsetThreshold / 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsSouthConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS1))
          ) >>>
          (TcsSouthConfig.gds ^|-> GuidersConfig.pwfs1).set(
            tag[P1Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        ) (baseConfig)
    ) shouldBe false
  }

  it should "decide if it can keep PWFS2 guiding active when applying an offset" in {
    //Big offset with PWFS2 in use
    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs2OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsSouthConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS2))
          ) >>>
          (TcsSouthConfig.gds ^|-> GuidersConfig.pwfs2).set(
            tag[P2Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        )(baseConfig)
    ) shouldBe true

    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs2OffsetThreshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsSouthConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideConfig.M1GuideOn(M1Source.PWFS2)
          ) >>>
          (TcsSouthConfig.gds ^|-> GuidersConfig.pwfs2).set(
            tag[P2Config](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          )
        )(baseConfig)
    ) shouldBe true

    //Small offset with PWFS2 in use
    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](TcsControllerEpics.pwfs2OffsetThreshold / 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsSouthConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS2))
          ) >>>
          (TcsSouthConfig.gds ^|-> GuidersConfig.pwfs2).set(
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
    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](threshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsSouthConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.OIWFS))
          ) >>>
          (TcsSouthConfig.gds ^|-> GuidersConfig.oiwfs).set(
            tag[OIConfig](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          ) >>>
          TcsSouthConfig.inst.set(DummyInstrument(threshold.some))
        )(baseConfig)
    ) shouldBe true

    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](threshold * 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsSouthConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideConfig.M1GuideOn(M1Source.OIWFS)
          ) >>>
          (TcsSouthConfig.gds ^|-> GuidersConfig.oiwfs).set(
            tag[OIConfig](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          ) >>>
          TcsSouthConfig.inst.set(DummyInstrument(threshold.some))
        )(baseConfig)
    ) shouldBe true

    //Small offset with OIWFS in use
    TcsSouthControllerEpics() invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (TcsSouthConfig.tc ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](threshold / 2.0 * FOCAL_PLANE_SCALE),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (TcsSouthConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.OIWFS))
          ) >>>
          (TcsSouthConfig.gds ^|-> GuidersConfig.oiwfs).set(
            tag[OIConfig](GuiderConfig(
              ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn
            ))
          ) >>>
          TcsSouthConfig.inst.set(DummyInstrument(threshold.some))
        )(baseConfig)
    ) shouldBe false
  }

}

object TcsSouthControllerEpicsSpec {
  final case class DummyInstrument(threshold: Option[Length]) extends InstrumentGuide {
    override val instrument: Instrument = Instrument.Gsaoi

    override def oiOffsetGuideThreshold: Option[Length] = threshold
  }
}
