// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.implicits._
import edu.gemini.spModel.core.Wavelength
import gem.enum.LightSinkName.Gmos
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should.Matchers._
import seqexec.model.enum._
import seqexec.model.TelescopeGuideConfig
import seqexec.model.M1GuideConfig
import seqexec.model.M2GuideConfig
import seqexec.server.InstrumentGuide
import seqexec.server.altair.AltairController
import seqexec.server.altair.AltairController.AltairOff
import seqexec.server.tcs.TcsController.LightSource.Sky
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsNorthControllerEpicsAo.EpicsTcsAoConfig
import shapeless.tag
import shapeless.tag.@@
import squants.space._
import org.scalatest.flatspec.AnyFlatSpec

class TcsNorthControllerEpicsAoSpec extends AnyFlatSpec with PrivateMethodTester {
  import TcsNorthControllerEpicsAoSpec._

  private val baseCurrentStatus = EpicsTcsAoConfig(
    BaseEpicsTcsConfig(
      Arcseconds(33.8),
      FocalPlaneOffset(tag[OffsetX](Millimeters(0.0)), tag[OffsetY](Millimeters(0.0))),
      Wavelength(Microns(400)),
      GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
      GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
      GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff),
      TelescopeGuideConfig(MountGuideOption.MountGuideOff, M1GuideConfig.M1GuideOff, M2GuideConfig.M2GuideOff),
      AoFold.Out,
      false,
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
    ),
    ProbeTrackingConfig.Off
  )

  private val baseConfig = AoTcsConfig[GuiderConfig@@AoGuide, AltairController.AltairConfig](
    TelescopeGuideConfig(MountGuideOption.MountGuideOff, M1GuideConfig.M1GuideOff, M2GuideConfig.M2GuideOff),
    TelescopeConfig(None, None),
    AoGuidersConfig[GuiderConfig@@AoGuide](
      tag[P1Config](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
      tag[AoGuide](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)),
      tag[OIConfig](GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff))
    ),
    AGConfig(LightPath(Sky, Gmos), None),
    AltairOff,
    DummyInstrument(None)
  )

  private val mustPauseWhileOffsetting = PrivateMethod[Boolean]('mustPauseWhileOffsetting)

  // The test uses only NIRI, although the threshold can be different for each instrument. The reason is that the
  // goal of the test is to check that it works in general, not to test for the specific threshold of every
  // instrument.
  it should "decide if it can keep Altair guiding active when applying an offset" in {
    val niriAoThreshold = Arcseconds(3.0)
    //Big offset with Altair in use
    TcsNorthControllerEpicsAo invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (AoTcsConfig.tc[GuiderConfig@@AoGuide, AltairController.AltairConfig] ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](niriAoThreshold * 2.0),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.GAOS))
          ) >>>
          (AoTcsConfig.gds ^|-> AoGuidersConfig.aoguide[GuiderConfig@@AoGuide]).set(
            tag[AoGuide](
              GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
            )
          ) >>>
          AoTcsConfig.inst.set(DummyInstrument(none))
      )(baseConfig)
    ) shouldBe true

    TcsNorthControllerEpicsAo invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (AoTcsConfig.tc[GuiderConfig@@AoGuide, AltairController.AltairConfig] ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](niriAoThreshold * 2.0),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(
            M1GuideConfig.M1GuideOn(M1Source.GAOS)
          ) >>>
          (AoTcsConfig.gds ^|-> AoGuidersConfig.aoguide[GuiderConfig@@AoGuide]).set(
            tag[AoGuide](
              GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
            )
          ) >>>
          AoTcsConfig.inst.set(DummyInstrument(none))
      )(baseConfig)
    ) shouldBe true

    //Small offset with Altair in use
    TcsNorthControllerEpicsAo invokePrivate mustPauseWhileOffsetting(
      baseCurrentStatus,
      (
        (AoTcsConfig.tc[GuiderConfig@@AoGuide, AltairController.AltairConfig] ^|-> TelescopeConfig.offsetA).set(
          InstrumentOffset(
            tag[OffsetP](niriAoThreshold / 2.0),
            tag[OffsetQ](Arcseconds(0.0))
          ).some
        ) >>>
          (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
            M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.GAOS))
          ) >>>
          (AoTcsConfig.gds ^|-> AoGuidersConfig.aoguide[GuiderConfig@@AoGuide]).set(
            tag[AoGuide](
              GuiderConfig(ProbeTrackingConfig.On(NodChopTrackingConfig.Normal), GuiderSensorOn)
            )
          ) >>>
          AoTcsConfig.inst.set(DummyInstrument(none))
      )(baseConfig)
    ) shouldBe false

  }

}

object TcsNorthControllerEpicsAoSpec {
  final case class DummyInstrument(threshold: Option[Length]) extends InstrumentGuide {
    override val instrument: Instrument = Instrument.Niri

    override def oiOffsetGuideThreshold: Option[Length] = threshold
  }
}
