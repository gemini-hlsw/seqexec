// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.syntax.all._
import lucuma.core.enums.GiapiStatusApply._
import lucuma.core.math.Coordinates
import lucuma.core.math.RightAscension
import lucuma.core.math.Declination
import seqexec.model.Conditions
import edu.gemini.spModel.gemini.ghost.GhostBinning
import scala.concurrent.duration._
import edu.gemini.spModel.target.env.ResolutionMode

/**
 * Tests GHOST Config typeclasses
 */
final class GhostSpec extends munit.DisciplineSuite with GhostArbitraries {
  // checkAll("Eq[GHOSTConfig]", EqTests[GhostConfig].eqv)

  val dec    = Declination.fromRadians(1.0).getOrElse(Declination.Max)
  val ra     = RightAscension.fromRadians(2.0)
  val coord1 = Coordinates(ra, dec)
  val dec2   = Declination.fromRadians(1.2).getOrElse(Declination.Max)
  val ra2    = RightAscension.fromRadians(0.3)
  val coord2 = Coordinates(ra2, dec2)

  test("binning") {
    val cfg = GhostConfig(
      "OBJECT",
      "science",
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Slow),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Fast),
      none,
      FiberAgitator.On,
      FiberAgitator.Off,
      "target".some,
      Coordinates.Zero.some,
      none,
      none,
      none,
      none,
      none,
      none,
      Nil,
      ResolutionMode.GhostStandard.some,
      Conditions.Best,
      None
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostRedBinningRcf.applyItem)),
                 "1".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostRedBinningCcf.applyItem)),
                 "1".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostBlueBinningRcf.applyItem)),
                 "1".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostBlueBinningCcf.applyItem)),
                 "1".some
    )
  }
  test("fiber agitator on/off for bias") {
    val cfg = GhostConfig(
      "BIAS",
      "bias",
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Slow),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Fast),
      none,
      FiberAgitator.On,
      FiberAgitator.Off,
      "target".some,
      Coordinates.Zero.some,
      none,
      none,
      none,
      none,
      none,
      none,
      Nil,
      ResolutionMode.GhostStandard.some,
      Conditions.Best,
      None
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator1.applyItem)),
                 "FA_DEMAND_ON".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator2.applyItem)),
                 "FA_DEMAND_OFF".some
    )
  }
  test("fiber agitator on/off") {
    val cfg = GhostConfig(
      "OBJECT",
      "science",
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Slow),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Fast),
      none,
      FiberAgitator.On,
      FiberAgitator.Off,
      "target".some,
      Coordinates.Zero.some,
      none,
      none,
      none,
      none,
      none,
      none,
      Nil,
      ResolutionMode.GhostStandard.some,
      Conditions.Best,
      None
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator1.applyItem)),
                 "FA_DEMAND_NONE".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator2.applyItem)),
                 "FA_DEMAND_NONE".some
    )
  }
  test("sru ifu1 ra/dec") {
    val cfg = GhostConfig(
      "OBJECT",
      "science",
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Slow),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Fast),
      none,
      FiberAgitator.Off,
      FiberAgitator.Off,
      "target".some,
      coord1.some,
      none,
      none,
      none,
      none,
      none,
      none,
      Nil,
      ResolutionMode.GhostStandard.some,
      Conditions.Best,
      None
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.type")),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.ra")),
                 ra.toAngle.toDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.dec")),
                 dec.toAngle.toSignedDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.bundle")),
                 "IFU_STDRES".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.target")),
                 "IFU_TARGET_OBJECT".some
    )
    // ifu2 not used
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.type")),
                 "IFU_DEMAND_PARK".some
    )
    assert(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.bundle")).isEmpty)
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.target")),
                 "IFU_TARGET_NONE".some
    )
  }
  test("sru ifu1/2 ra/dec") {
    val cfg = GhostConfig(
      "OBJECT",
      "science",
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Slow),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Fast),
      none,
      FiberAgitator.Off,
      FiberAgitator.Off,
      "target".some,
      coord1.some,
      "target2".some,
      coord2.some,
      none,
      none,
      none,
      none,
      Nil,
      ResolutionMode.GhostStandard.some,
      Conditions.Best,
      None
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.type")),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.ra")),
                 ra.toAngle.toDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.dec")),
                 dec.toAngle.toSignedDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.bundle")),
                 "IFU_STDRES".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.target")),
                 "IFU_TARGET_OBJECT".some
    )
    // ifu2 not used
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.type")),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.bundle")),
                 "IFU_STDRES".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.target")),
                 "IFU_TARGET_OBJECT".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.ra")),
                 ra2.toAngle.toDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.dec")),
                 dec2.toAngle.toSignedDoubleDegrees.some.map(v => f"$v%1.6f")
    )
  }
  test("sru ifu1 park if only one used") {
    val cfg = GhostConfig(
      "OBJECT",
      "science",
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Slow),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Fast),
      none,
      FiberAgitator.Off,
      FiberAgitator.Off,
      "target".some,
      coord1.some,
      none,
      none,
      none,
      none,
      none,
      none,
      Nil,
      ResolutionMode.GhostStandard.some,
      Conditions.Best,
      None
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.type")),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.ra")),
                 ra.toAngle.toDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.dec")),
                 dec.toAngle.toSignedDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.bundle")),
                 "IFU_STDRES".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.target")),
                 "IFU_TARGET_OBJECT".some
    )
    // ifu2 not used
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.type")),
                 "IFU_DEMAND_PARK".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.bundle")), None)
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.target")),
                 "IFU_TARGET_NONE".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.ra")), None)
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.dec")), None)
  }
  test("hru ifu1 ra/dec") {
    val cfg = GhostConfig(
      "OBJECT",
      "science",
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Slow),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Fast),
      none,
      FiberAgitator.Off,
      FiberAgitator.Off,
      none,
      none,
      none,
      none,
      "target".some,
      coord1.some,
      none,
      none,
      Nil,
      ResolutionMode.GhostStandard.some,
      Conditions.Best,
      None
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.type")),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.ra")),
                 ra.toAngle.toDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.dec")),
                 dec.toAngle.toSignedDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.bundle")),
                 "IFU_HIRES".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.target")),
                 "IFU_TARGET_OBJECT".some
    )
    // ifu2 not used
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.type")),
                 "IFU_DEMAND_PARK".some
    )
    assert(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.bundle")).isEmpty)
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.target")),
                 "IFU_TARGET_NONE".some
    )
  }
  test("hru ifu2 ra/dec") {
    val cfg = GhostConfig(
      "OBJECT",
      "science",
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Slow),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1, ReadNoiseGain.Fast),
      none,
      FiberAgitator.Off,
      FiberAgitator.Off,
      none,
      none,
      none,
      none,
      none,
      none,
      "target2".some,
      coord2.some,
      Nil,
      ResolutionMode.GhostStandard.some,
      Conditions.Best,
      None
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.type")),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.ra")),
                 ra2.toAngle.toDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.dec")),
                 dec2.toAngle.toSignedDoubleDegrees.some.map(v => f"$v%1.6f")
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.bundle")),
                 "IFU_HIRES".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.target")),
                 "IFU_TARGET_OBJECT".some
    )
    // ifu2 not used
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.type")),
                 "IFU_DEMAND_PARK".some
    )
    assert(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.bundle")).isEmpty)
    assertEquals(cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.target")),
                 "IFU_TARGET_NONE".some
    )
  }
}
