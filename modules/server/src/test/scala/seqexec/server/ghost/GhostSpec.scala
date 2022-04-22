// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.syntax.all._
import cats.kernel.laws.discipline._
import lucuma.core.enum.GiapiStatusApply._
import lucuma.core.math.Coordinates
import lucuma.core.math.RightAscension
import lucuma.core.math.Declination
import edu.gemini.spModel.gemini.ghost.GhostBinning
import scala.concurrent.duration._

/**
 * Tests GHOST Config typeclasses
 */
final class GhostSpec extends munit.DisciplineSuite with GhostArbitraries {
  // checkAll("Eq[GHOSTConfig]", EqTests[GhostConfig].eqv)

  val dec   = Declination.fromRadians(1.0).getOrElse(Declination.Max)
  val ra    = RightAscension.fromRadians(2.0)
  val coord = Coordinates(ra, dec)

  test("binning") {
    val cfg = GhostConfig(
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1),
      ChannelConfig(GhostBinning.TWO_BY_TWO, 1.seconds, 1),
      none,
      1.seconds,
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
      Nil
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostRedBinningRcf.applyItem)),
                 "2".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostRedBinningCcf.applyItem)),
                 "2".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostBlueBinningRcf.applyItem)),
                 "1".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostBlueBinningCcf.applyItem)),
                 "1".some
    )
  }
  test("fiber agitator on/off") {
    val cfg = GhostConfig(
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1),
      none,
      1.seconds,
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
      Nil
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator1.applyItem)),
                 "FA_DEMAND_ON".some
    )
    assertEquals(cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator2.applyItem)),
                 "FA_DEMAND_OFF".some
    )
  }
  test("sru ifu1 ra/dec") {
    val cfg = GhostConfig(
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1),
      ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1),
      none,
      1.seconds,
      FiberAgitator.Off,
      FiberAgitator.Off,
      "target".some,
      coord.some,
      none,
      none,
      none,
      none,
      none,
      none,
      Nil
    )
    println(cfg.map(_.configuration))
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
}
