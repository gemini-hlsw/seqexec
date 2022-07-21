// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.syntax.all._
import cats.effect.IO
import edu.gemini.seqexec.server.altair.LgsSfoControl
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import seqexec.model.`enum`.Instrument
import seqexec.server.tcs.TcsController.{
  FocalPlaneOffset,
  InstrumentOffset,
  OffsetP,
  OffsetQ,
  OffsetX,
  OffsetY
}
import seqexec.server.tcs.TestTcsEpics.TestTcsEvent.AoCorrectCmd
import seqexec.server.tcs.{ Gaos, TestTcsEpics }
import shapeless.tag
import squants.space.LengthConversions._
import squants.space.AngleConversions._

class AltairControllerEpicsSpec extends munit.CatsEffectSuite {

  private implicit def unsafeLogger: Logger[IO] = NoOpLogger.impl[IO]

  private val aoGuidedTcs = TestTcsEpics.defaultState.copy(
    m1GuideSource = "GAOS",
    m2aoGuide = "ON",
    absorbTipTilt = 1,
    comaCorrect = "On",
    aoFollowS = "On"
  )

  private val baseOffset =
    FocalPlaneOffset(tag[OffsetX](0.0.millimeters), tag[OffsetY](0.0.millimeters))

  test("AltairControllerEpics should not pause nor resume NGS AO guiding if nothing has changed") {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val tcsEpics = TestTcsEpics.build[IO](aoGuidedTcs)

    val altairCfg = AltairController.Ngs(blend = false, (0.0.millimeters, 0.0.millimeters))

    for {
      ao   <- altairEpics
      tcs  <- tcsEpics
      c     = AltairControllerEpics(ao, tcs)
      r    <- c.pauseResume(
                Gaos.PauseConditionSet.empty,
                Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.GaosGuideOn,
                baseOffset,
                Instrument.Gnirs
              )(altairCfg)
      _    <- r.pause.orEmpty
      ao1  <- ao.outputF
      tcs1 <- tcs.outputF
      _    <- ao.out.set(List.empty) *> tcs.out.set(List.empty)
      _    <- r.resume.orEmpty
      ao2  <- ao.outputF
      tcs2 <- tcs.outputF
    } yield {
      assert(ao1.isEmpty)
      assert(tcs1.isEmpty)
      assert(ao2.isEmpty)
      assert(tcs2.isEmpty)
      assert(!r.forceFreeze)
      assert(r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
      assert(r.restoreOnResume.canGuideM2)
      assert(r.restoreOnResume.canGuideM1)
    }
  }

  test("AltairControllerEpics should pause and resume NGS AO guiding for big enough offsets") {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val tcsEpics = TestTcsEpics.build[IO](aoGuidedTcs)

    val altairCfg = AltairController.Ngs(blend = false, (0.0.millimeters, 0.0.millimeters))

    val offset = FocalPlaneOffset(tag[OffsetX](10.0.millimeters), tag[OffsetY](10.0.millimeters))

    for {
      ao   <- altairEpics
      tcs  <- tcsEpics
      c     = AltairControllerEpics(ao, tcs)
      r    <- c.pauseResume(
                Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(baseOffset, offset),
                Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.GaosGuideOn + Gaos.ResumeCondition
                  .OffsetReached(offset),
                baseOffset,
                Instrument.Gnirs
              )(altairCfg)
      _    <- r.pause.orEmpty
      ao1  <- ao.outputF
      tcs1 <- tcs.outputF
      _    <- ao.out.set(List.empty) *> tcs.out.set(List.empty)
      _    <- r.resume.orEmpty
      ao2  <- ao.outputF
      tcs2 <- tcs.outputF
    } yield {
      assert(ao1.isEmpty)
      assert(tcs1.contains(AoCorrectCmd("OFF", 0)))
      assert(ao2.isEmpty)
      assert(tcs2.contains(AoCorrectCmd("ON", 1)))
      assert(!r.forceFreeze)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(!r.guideWhilePaused.canGuideM1)
      assert(r.restoreOnResume.canGuideM2)
      assert(r.restoreOnResume.canGuideM1)
    }
  }

  test("AltairControllerEpics should keep target filter enabled for small offsets") {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val tcsEpics = TestTcsEpics.build[IO](aoGuidedTcs)

    val altairCfg = AltairController.Ngs(blend = false, (0.0.millimeters, 0.0.millimeters))

    val offset = InstrumentOffset(tag[OffsetP](1.0.arcseconds), tag[OffsetQ](1.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(baseOffset, offset),
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.GaosGuideOn + Gaos.ResumeCondition
                 .OffsetReached(offset),
               baseOffset,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(!r.pauseTargetFilter)
      assert(r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

  test("AltairControllerEpics should disable and enable target filter when applying a sky offset") {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = false,
        aoFollow = true,
        aoSettled = true
      )
    )

    val skyOffset = InstrumentOffset(tag[OffsetP](30.0.arcseconds), tag[OffsetQ](30.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    val tcsEpics = TestTcsEpics.build[IO](
      aoGuidedTcs.copy(xoffsetPoA1 = baseOffset.x.toMillimeters,
                       yoffsetPoA1 = baseOffset.y.toMillimeters
      )
    )

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (0.0.millimeters, 0.0.millimeters))

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(baseOffset,
                                                                             skyOffset
               ) + Gaos.PauseCondition.GaosGuideOff,
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition
                 .OffsetReached(skyOffset),
               baseOffset,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(r.pauseTargetFilter)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

  test(
    "AltairControllerEpics should disable and enable target filter when moving between two skies"
  ) {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = false,
        aoFollow = true,
        aoSettled = true
      )
    )

    val skyOffset0 = InstrumentOffset(tag[OffsetP](30.0.arcseconds), tag[OffsetQ](30.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    val tcsEpics = TestTcsEpics.build[IO](
      aoGuidedTcs.copy(xoffsetPoA1 = skyOffset0.x.toMillimeters,
                       yoffsetPoA1 = skyOffset0.y.toMillimeters
      )
    )

    val altairCfg  =
      AltairController.Lgs(strap = true, sfo = true, (skyOffset0.x, skyOffset0.y))
    val skyOffset1 =
      InstrumentOffset(tag[OffsetP](-30.0.arcseconds), tag[OffsetQ](-30.0.arcseconds))
        .toFocalPlaneOffset(0.0.arcseconds)

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(skyOffset0,
                                                                             skyOffset1
               ) + Gaos.PauseCondition.GaosGuideOff,
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition
                 .OffsetReached(skyOffset1),
               skyOffset0,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(r.pauseTargetFilter)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

  test(
    "AltairControllerEpics should disable and enable target filter when coming from a sky offset"
  ) {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = false,
        aoFollow = false,
        aoSettled = true
      )
    )

    val skyOffset = InstrumentOffset(tag[OffsetP](30.0.arcseconds), tag[OffsetQ](30.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    val tcsEpics = TestTcsEpics.build[IO](
      aoGuidedTcs.copy(xoffsetPoA1 = skyOffset.x.toMillimeters,
                       yoffsetPoA1 = skyOffset.y.toMillimeters
      )
    )

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (skyOffset.x, skyOffset.y))

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(skyOffset, baseOffset),
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.GaosGuideOn + Gaos.ResumeCondition
                 .OffsetReached(baseOffset),
               skyOffset,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(r.pauseTargetFilter)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

  test("AltairControllerEpics should disable and enable target filter when applying a big offset") {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val skyOffset = InstrumentOffset(tag[OffsetP](30.0.arcseconds), tag[OffsetQ](30.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    val tcsEpics = TestTcsEpics.build[IO](
      aoGuidedTcs.copy(xoffsetPoA1 = baseOffset.x.toMillimeters,
                       yoffsetPoA1 = baseOffset.y.toMillimeters
      )
    )

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (0.0.millimeters, 0.0.millimeters))

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(baseOffset, skyOffset),
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.GaosGuideOn + Gaos.ResumeCondition
                 .OffsetReached(skyOffset),
               baseOffset,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(r.pauseTargetFilter)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

  test(
    "AltairControllerEpics should disable and enable target filter when coming back from big offset"
  ) {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val skyOffset = InstrumentOffset(tag[OffsetP](30.0.arcseconds), tag[OffsetQ](30.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    val tcsEpics = TestTcsEpics.build[IO](
      aoGuidedTcs.copy(xoffsetPoA1 = skyOffset.x.toMillimeters,
                       yoffsetPoA1 = skyOffset.y.toMillimeters
      )
    )

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (skyOffset.x, skyOffset.y))

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(skyOffset, baseOffset),
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.GaosGuideOn + Gaos.ResumeCondition
                 .OffsetReached(baseOffset),
               skyOffset,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(r.pauseTargetFilter)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

  test("AltairControllerEpics should pause and not resume NGS AO for unguided step") {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val tcsEpics = TestTcsEpics.build[IO](aoGuidedTcs)

    val altairCfg = AltairController.Ngs(blend = false, (0.0.millimeters, 0.0.millimeters))

    for {
      ao   <- altairEpics
      tcs  <- tcsEpics
      c     = AltairControllerEpics(ao, tcs)
      r    <- c.pauseResume(
                Gaos.PauseConditionSet.empty + Gaos.PauseCondition.GaosGuideOff,
                Gaos.ResumeConditionSet.empty,
                baseOffset,
                Instrument.Gnirs
              )(altairCfg)
      _    <- r.pause.orEmpty
      ao1  <- ao.outputF
      tcs1 <- tcs.outputF
      _    <- ao.out.set(List.empty) *> tcs.out.set(List.empty)
      _    <- r.resume.orEmpty
      ao2  <- ao.outputF
      tcs2 <- tcs.outputF
    } yield {
      assert(ao1.isEmpty)
      assert(tcs1.contains(AoCorrectCmd("OFF", 0)))
      assert(ao2.isEmpty)
      assert(!tcs2.contains(AoCorrectCmd("ON", 1)))
      assert(!r.forceFreeze)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(!r.guideWhilePaused.canGuideM1)
      assert(!r.restoreOnResume.canGuideM2)
      assert(!r.restoreOnResume.canGuideM1)
      assert(!r.pauseTargetFilter)
    }
  }

  test("AltairControllerEpics should pause and not resume NGS AO for unguided step with offset") {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val tcsEpics = TestTcsEpics.build[IO](aoGuidedTcs)

    val altairCfg = AltairController.Ngs(blend = false, (0.0.millimeters, 0.0.millimeters))

    val offset = FocalPlaneOffset(tag[OffsetX](10.0.millimeters), tag[OffsetY](10.0.millimeters))

    for {
      ao   <- altairEpics
      tcs  <- tcsEpics
      c     = AltairControllerEpics(ao, tcs)
      r    <- c.pauseResume(
                Gaos.PauseConditionSet.empty + Gaos.PauseCondition.GaosGuideOff + Gaos.PauseCondition
                  .OffsetMove(baseOffset, offset),
                Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.OffsetReached(offset),
                baseOffset,
                Instrument.Gnirs
              )(altairCfg)
      _    <- r.pause.orEmpty
      ao1  <- ao.outputF
      tcs1 <- tcs.outputF
      _    <- ao.out.set(List.empty) *> tcs.out.set(List.empty)
      _    <- r.resume.orEmpty
      ao2  <- ao.outputF
      tcs2 <- tcs.outputF
    } yield {
      assert(ao1.isEmpty)
      assert(tcs1.contains(AoCorrectCmd("OFF", 0)))
      assert(ao2.isEmpty)
      assert(!tcs2.contains(AoCorrectCmd("ON", 1)))
      assert(!r.forceFreeze)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(!r.guideWhilePaused.canGuideM1)
      assert(!r.restoreOnResume.canGuideM2)
      assert(!r.restoreOnResume.canGuideM1)
      assert(r.pauseTargetFilter)
    }
  }

  test(
    "AltairControllerEpics should control sfo and strap for LGS AO on big offsets, but not pause AO"
  ) {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true,
        strapLoop = true,
        sfoLoop = LgsSfoControl.Enable,
        strapGate = 1,
        strapHVStatus = true,
        strapRTStatus = true,
        strapTempStatus = true
      )
    )

    val tcsEpics = TestTcsEpics.build[IO](aoGuidedTcs)

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (0.0.millimeters, 0.0.millimeters))

    val offset = FocalPlaneOffset(tag[OffsetX](10.0.millimeters), tag[OffsetY](10.0.millimeters))

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(baseOffset, offset),
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.GaosGuideOn + Gaos.ResumeCondition
                 .OffsetReached(offset),
               baseOffset,
               Instrument.Gnirs
             )(altairCfg)
      _   <- r.pause.orEmpty
      ao1 <- ao.outputF
      // tcs1 <- tcs.outputF
      _   <- ao.out.set(List.empty) *> tcs.out.set(List.empty)
      _   <- r.resume.orEmpty
      ao2 <- ao.outputF
      // tcs2 <- tcs.outputF
    } yield {
      assert(ao1.contains(TestAltairEpics.Event.StrapGateControlCmd(0)))
      assert(ao1.contains(TestAltairEpics.Event.StrapControlCmd(0)))
      assert(ao1.contains(TestAltairEpics.Event.SfoControlCmd(LgsSfoControl.Pause)))
      // assert(!tcs1.contains(AoCorrectCmd("OFF", 0)))
      assert(ao2.contains(TestAltairEpics.Event.StrapGateControlCmd(1)))
      assert(ao2.contains(TestAltairEpics.Event.StrapControlCmd(1)))
      assert(ao2.contains(TestAltairEpics.Event.SfoControlCmd(LgsSfoControl.Enable)))
      // assert(!tcs2.contains(AoCorrectCmd("ON", 1)))
      assert(!r.forceFreeze)
      assert(!r.guideWhilePaused.canGuideM2)
      // assert(r.guideWhilePaused.canGuideM1)
      assert(r.restoreOnResume.canGuideM2)
      assert(r.restoreOnResume.canGuideM1)
      assert(r.pauseTargetFilter)
    }
  }

  test(
    "AltairControllerEpics should disable and enable target filter for LGS mode when applying a sky offset."
  ) {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true,
        strapLoop = false,
        sfoLoop = LgsSfoControl.Disable,
        strapGate = 1,
        strapHVStatus = true,
        strapRTStatus = true,
        strapTempStatus = true
      )
    )

    val skyOffset = InstrumentOffset(tag[OffsetP](30.0.arcseconds), tag[OffsetQ](30.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    val tcsEpics = TestTcsEpics.build[IO](
      aoGuidedTcs.copy(xoffsetPoA1 = baseOffset.x.toMillimeters,
                       yoffsetPoA1 = baseOffset.y.toMillimeters
      )
    )

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (0.0.millimeters, 0.0.millimeters))

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(baseOffset,
                                                                             skyOffset
               ) + Gaos.PauseCondition.GaosGuideOff,
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.OffsetReached(skyOffset),
               baseOffset,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(r.pauseTargetFilter)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

  test(
    "AltairControllerEpics should disable and enable target filter for LGS mode when moving between two skies."
  ) {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true,
        strapLoop = false,
        sfoLoop = LgsSfoControl.Disable,
        strapGate = 1,
        strapHVStatus = true,
        strapRTStatus = true,
        strapTempStatus = true
      )
    )

    val skyOffset0 = InstrumentOffset(tag[OffsetP](30.0.arcseconds), tag[OffsetQ](30.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    val tcsEpics = TestTcsEpics.build[IO](
      aoGuidedTcs.copy(xoffsetPoA1 = skyOffset0.x.toMillimeters,
                       yoffsetPoA1 = skyOffset0.y.toMillimeters
      )
    )

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (skyOffset0.x, skyOffset0.y))

    val skyOffset1 =
      InstrumentOffset(tag[OffsetP](-30.0.arcseconds), tag[OffsetQ](-30.0.arcseconds))
        .toFocalPlaneOffset(0.0.arcseconds)

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(skyOffset0, skyOffset1),
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition
                 .OffsetReached(skyOffset1),
               skyOffset0,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(r.pauseTargetFilter)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

  test(
    "AltairControllerEpics should disable and enable target filter for LGS mode when coming back from big offset."
  ) {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true,
        strapLoop = false,
        sfoLoop = LgsSfoControl.Disable,
        strapGate = 1,
        strapHVStatus = true,
        strapRTStatus = true,
        strapTempStatus = true
      )
    )

    val skyOffset = InstrumentOffset(tag[OffsetP](30.0.arcseconds), tag[OffsetQ](30.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    val tcsEpics = TestTcsEpics.build[IO](
      aoGuidedTcs.copy(xoffsetPoA1 = skyOffset.x.toMillimeters,
                       yoffsetPoA1 = skyOffset.y.toMillimeters
      )
    )

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (0.0.millimeters, 0.0.millimeters))

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(skyOffset, baseOffset),
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.GaosGuideOn + Gaos.ResumeCondition
                 .OffsetReached(baseOffset),
               skyOffset,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(r.pauseTargetFilter)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

  test(
    "AltairControllerEpics should keep target filter enabled for LGS mode when applying small offsets."
  ) {
    val altairEpics = TestAltairEpics.build[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true,
        strapLoop = true,
        sfoLoop = LgsSfoControl.Enable,
        strapGate = 1,
        strapHVStatus = true,
        strapRTStatus = true,
        strapTempStatus = true
      )
    )

    val tcsEpics = TestTcsEpics.build[IO](aoGuidedTcs)

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (0.0.millimeters, 0.0.millimeters))

    val offset = InstrumentOffset(tag[OffsetP](1.0.arcseconds), tag[OffsetQ](1.0.arcseconds))
      .toFocalPlaneOffset(0.0.arcseconds)

    for {
      ao  <- altairEpics
      tcs <- tcsEpics
      c    = AltairControllerEpics(ao, tcs)
      r   <- c.pauseResume(
               Gaos.PauseConditionSet.empty + Gaos.PauseCondition.OffsetMove(baseOffset, offset),
               Gaos.ResumeConditionSet.empty + Gaos.ResumeCondition.GaosGuideOn + Gaos.ResumeCondition
                 .OffsetReached(offset),
               baseOffset,
               Instrument.Gnirs
             )(altairCfg)
    } yield {
      assert(!r.pauseTargetFilter)
      assert(r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
    }
  }

}
