// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.syntax.all._
import cats.effect.{ IO, Sync }
import edu.gemini.seqexec.server.altair.LgsSfoControl
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import seqexec.model.`enum`.Instrument
import seqexec.server.tcs.TcsController.{ FocalPlaneOffset, OffsetX, OffsetY }
import seqexec.server.tcs.TestTcsEpics.TestTcsEvent.AoCorrectCmd
import seqexec.server.tcs.{ Gaos, TestTcsEpics }
import shapeless.tag
import squants.space.LengthConversions._
import cats.effect.Ref

class AltairControllerEpicsSpec extends munit.CatsEffectSuite {
  import AltairControllerEpicsSpec._

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
    val altairEpics = buildAltairController[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val tcsEpics = buildTcsController[IO](aoGuidedTcs)

    val altairCfg = AltairController.Ngs(false, (0.0.millimeters, 0.0.millimeters))

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
    val altairEpics = buildAltairController[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val tcsEpics = buildTcsController[IO](aoGuidedTcs)

    val altairCfg = AltairController.Ngs(false, (0.0.millimeters, 0.0.millimeters))

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

  test("AltairControllerEpics should pause and not resume NGS AO for unguided step") {
    val altairEpics = buildAltairController[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val tcsEpics = buildTcsController[IO](aoGuidedTcs)

    val altairCfg = AltairController.Ngs(false, (0.0.millimeters, 0.0.millimeters))

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
    }
  }

  test("AltairControllerEpics should pause and not resume NGS AO for unguided step with offset") {
    val altairEpics = buildAltairController[IO](
      TestAltairEpics.defaultState.copy(
        aoLoop = true,
        aoFollow = true,
        aoSettled = true
      )
    )

    val tcsEpics = buildTcsController[IO](aoGuidedTcs)

    val altairCfg = AltairController.Ngs(false, (0.0.millimeters, 0.0.millimeters))

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
    }
  }

  test(
    "AltairControllerEpics should control sfo and strap for LGS AO on big offsets, but not pause AO"
  ) {
    val altairEpics = buildAltairController[IO](
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

    val tcsEpics = buildTcsController[IO](aoGuidedTcs)

    val altairCfg =
      AltairController.Lgs(strap = true, sfo = true, (0.0.millimeters, 0.0.millimeters))

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
      assert(ao1.contains(TestAltairEpics.Event.StrapGateControlCmd(0)))
      assert(ao1.contains(TestAltairEpics.Event.StrapControlCmd(0)))
      assert(ao1.contains(TestAltairEpics.Event.SfoControlCmd(LgsSfoControl.Pause)))
      assert(!tcs1.contains(AoCorrectCmd("OFF", 0)))
      assert(ao2.contains(TestAltairEpics.Event.StrapGateControlCmd(1)))
      assert(ao2.contains(TestAltairEpics.Event.StrapControlCmd(1)))
      assert(ao2.contains(TestAltairEpics.Event.SfoControlCmd(LgsSfoControl.Enable)))
      assert(!tcs2.contains(AoCorrectCmd("ON", 1)))
      assert(!r.forceFreeze)
      assert(!r.guideWhilePaused.canGuideM2)
      assert(r.guideWhilePaused.canGuideM1)
      assert(r.restoreOnResume.canGuideM2)
      assert(r.restoreOnResume.canGuideM1)
    }
  }

}

object AltairControllerEpicsSpec {
  def buildAltairController[F[_]: Sync](baseState: TestAltairEpics.State): F[TestAltairEpics[F]] =
    for {
      st  <- Ref.of(baseState)
      out <- Ref.of(List.empty[TestAltairEpics.Event])
    } yield TestAltairEpics[F](st, out)

  def buildTcsController[F[_]: Sync](baseState: TestTcsEpics.State): F[TestTcsEpics[F]] =
    for {
      stR  <- Ref.of[F, TestTcsEpics.State](baseState)
      outR <- Ref.of[F, List[TestTcsEpics.TestTcsEvent]](List.empty)
    } yield TestTcsEpics[F](stR, outR)

}
