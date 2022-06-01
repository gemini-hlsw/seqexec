// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.syntax.all._
import cats.effect.IO
import edu.gemini.seqexec.server.gems.LoopState
import munit.CatsEffectSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import seqexec.server.gems.GemsController.{
  Cwfs1Usage,
  Cwfs2Usage,
  Cwfs3Usage,
  OIUsage,
  Odgw1Usage,
  Odgw2Usage,
  Odgw3Usage,
  Odgw4Usage,
  P1Usage
}
import seqexec.server.gems.GemsControllerEpicsSpec.DummyGsaoiGuider
import seqexec.server.gems.TestGemsEpics.LoopEvent
import seqexec.server.gsaoi.GsaoiGuider
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.{ PauseCondition, ResumeCondition }
import seqexec.server.tcs.TcsController.{ FocalPlaneOffset, OffsetX, OffsetY }
import shapeless.tag
import squants.space.LengthConversions._

class GemsControllerEpicsSpec extends CatsEffectSuite {

  private implicit def unsafeLogger: Logger[IO] = NoOpLogger.impl[IO]

  val guidingState = TestGemsEpics.defaultState.copy(
    aniLoop = LoopState.CLOSED,
    ttLoop = LoopState.CLOSED,
    flexureLoop = LoopState.CLOSED,
    focusLoop = LoopState.CLOSED
  )

  val guidingConfig = GemsController.GemsOn(
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

  private val baseOffset =
    FocalPlaneOffset(tag[OffsetX](0.0.millimeters), tag[OffsetY](0.0.millimeters))

  private val ditherOffset =
    FocalPlaneOffset(tag[OffsetX](3.0.millimeters), tag[OffsetY](-3.0.millimeters))

  test("GemsControlerEpics should do nothing if there is no change in configuration") {
    val gemsEpicsF = TestGemsEpics.build[IO](guidingState)

    for {
      ge <- gemsEpicsF
      cc  = GemsControllerEpics(ge, DummyGsaoiGuider)
      pr <-
        cc.pauseResume(Gaos.PauseConditionSet.empty, Gaos.ResumeConditionSet.empty)(guidingConfig)
      _  <- pr.pause.orEmpty
      l1 <- ge.outputF
      _  <- ge.out.set(List.empty)
      _  <- pr.resume.orEmpty
      l2 <- ge.outputF
    } yield {
      assert(l1.isEmpty)
      assert(l2.isEmpty)
    }
  }

  test("GemsControlerEpics should pause and resume guiding on offsets") {
    val gemsEpicsF = TestGemsEpics.build[IO](guidingState)

    for {
      ge <- gemsEpicsF
      cc  = GemsControllerEpics(ge, DummyGsaoiGuider)
      pr <- cc.pauseResume(
              Gaos.PauseConditionSet.empty + PauseCondition.OffsetMove(baseOffset, ditherOffset),
              Gaos.ResumeConditionSet.empty + ResumeCondition.OffsetReached(ditherOffset)
            )(guidingConfig)
      _  <- pr.pause.orEmpty
      l1 <- ge.outputF
      _  <- ge.out.set(List.empty)
      _  <- pr.resume.orEmpty
      l2 <- ge.outputF
    } yield {
      assertEquals(l1.length, 1)
      assertEquals(l1.head,
                   LoopEvent(GemsControllerEpics.PauseCmd, GemsControllerEpics.OffsetCondition)
      )
      assertEquals(l2.length, 1)
      assertEquals(l2.head,
                   LoopEvent(GemsControllerEpics.ResumeCmd, GemsControllerEpics.OffsetCondition)
      )
    }
  }

  test("GemsControlerEpics should pause guiding on a sky") {
    val gemsEpicsF = TestGemsEpics.build[IO](guidingState)

    for {
      ge <- gemsEpicsF
      cc  = GemsControllerEpics(ge, DummyGsaoiGuider)
      pr <- cc.pauseResume(Gaos.PauseConditionSet.empty + PauseCondition.GaosGuideOff,
                           Gaos.ResumeConditionSet.empty
            )(guidingConfig)
      _  <- pr.pause.orEmpty
      l1 <- ge.outputF
      _  <- ge.out.set(List.empty)
      _  <- pr.resume.orEmpty
      l2 <- ge.outputF
    } yield {
      assertEquals(l1.length, 1)
      assertEquals(l1.head,
                   LoopEvent(GemsControllerEpics.PauseCmd, GemsControllerEpics.UnguidedCondition)
      )
      assert(l2.isEmpty)
    }
  }

  test("GemsControlerEpics should resume guiding after an sky") {
    val gemsEpicsF = TestGemsEpics.build[IO](
      TestGemsEpics.defaultState.copy(
        aniLoop = LoopState.PAUSED,
        ttLoop = LoopState.PAUSED,
        flexureLoop = LoopState.PAUSED,
        focusLoop = LoopState.PAUSED,
        pauseReasons = Set(TestGemsEpics.Sky)
      )
    )

    for {
      ge <- gemsEpicsF
      cc  = GemsControllerEpics(ge, DummyGsaoiGuider)
      pr <- cc.pauseResume(
              Gaos.PauseConditionSet.empty + PauseCondition.OffsetMove(ditherOffset, baseOffset),
              Gaos.ResumeConditionSet.empty + ResumeCondition.OffsetReached(
                baseOffset
              ) + ResumeCondition.GaosGuideOn
            )(guidingConfig)
      _  <- pr.pause.orEmpty
      l1 <- ge.outputF
      _  <- ge.out.set(List.empty)
      _  <- pr.resume.orEmpty
      l2 <- ge.outputF
      r2  = l2.flatMap { case LoopEvent(_, s) => s.split("\\|").map(_.trim).toList }
    } yield {
      assertEquals(l1.length, 1)
      assertEquals(l1.head,
                   LoopEvent(GemsControllerEpics.PauseCmd, GemsControllerEpics.OffsetCondition)
      )
      assertEquals(l2.length, 1)
      assertEquals(l2.head.cmd, GemsControllerEpics.ResumeCmd)
      assertEquals(r2.length, 2)
      assert(
        r2.contains(GemsControllerEpics.UnguidedCondition) && r2.contains(
          GemsControllerEpics.OffsetCondition
        )
      )
    }
  }

  test("GemsControlerEpics should properly handle offset to unguided position") {
    val gemsEpicsF = TestGemsEpics.build[IO](guidingState)

    for {
      ge <- gemsEpicsF
      cc  = GemsControllerEpics(ge, DummyGsaoiGuider)
      pr <- cc.pauseResume(
              Gaos.PauseConditionSet.empty + PauseCondition.OffsetMove(baseOffset,
                                                                       ditherOffset
              ) + PauseCondition.GaosGuideOff,
              Gaos.ResumeConditionSet.empty + ResumeCondition.OffsetReached(ditherOffset)
            )(guidingConfig)
      _  <- pr.pause.orEmpty
      l1 <- ge.outputF
      r1  = l1.flatMap { case LoopEvent(_, s) => s.split("\\|").map(_.trim).toList }
      _  <- ge.out.set(List.empty)
      _  <- pr.resume.orEmpty
      l2 <- ge.outputF
    } yield {
      assertEquals(l1.length, 1)
      assertEquals(l1.head.cmd, GemsControllerEpics.PauseCmd)
      assertEquals(r1.length, 2)
      assert(
        r1.contains(GemsControllerEpics.UnguidedCondition) && r1.contains(
          GemsControllerEpics.OffsetCondition
        )
      )
      assertEquals(l2.length, 1)
      assertEquals(l2.head,
                   LoopEvent(GemsControllerEpics.ResumeCmd, GemsControllerEpics.OffsetCondition)
      )
    }
  }

}

object GemsControllerEpicsSpec {
  object DummyGsaoiGuider extends GsaoiGuider[IO] {
    override def currentState: IO[GsaoiGuider.GuideState] = new GsaoiGuider.GuideState {
      override def isGuideActive: Boolean = false

      override def isOdgwGuiding(odgwId: GsaoiGuider.OdgwId): Boolean = false
    }.pure[IO]

    override def guide: IO[Unit] = IO.unit

    override def endGuide: IO[Unit] = IO.unit
  }
}
