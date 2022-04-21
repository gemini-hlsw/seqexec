// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import lucuma.core.enum.GiapiStatusApply._
import lucuma.core.math.Coordinates
import lucuma.core.math.RightAscension
import lucuma.core.math.Declination
import edu.gemini.spModel.gemini.ghost.GhostBinning
import org.scalatest.EitherValues
import scala.concurrent.duration._

/**
 * Tests GHOST Config typeclasses
 */
final class GhostSpec extends CatsSuite with GhostArbitraries with EitherValues {
  // checkAll("Eq[GHOSTConfig]", EqTests[GhostConfig].eqv)

  val dec   = Declination.fromRadians(1.0).getOrElse(Declination.Max)
  val ra    = RightAscension.fromRadians(2.0)
  val coord = Coordinates(ra, dec)

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
    println(cfg.toOption.map(_.configuration))
    cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator1.applyItem)) shouldBe Some(
      "FA_DEMAND_ON"
    )
    cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator2.applyItem)) shouldBe Some(
      "FA_DEMAND_OFF"
    )
  }
  // test("fiber agitator off") {
  //   val cfg = GhostConfig(
  //     ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1),
  //     ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1),
  //     none,
  //     1.seconds,
  //     FiberAgitator.Off,
  //     FiberAgitator.Off,
  //     "target".some,
  //     Coordinates.Zero.some,
  //     none,
  //     none,
  //     none,
  //     none,
  //     none,
  //     none,
  //     Nil
  //   )
  //   cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator1.applyItem)) shouldBe Some("0")
  //   cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator2.applyItem)) shouldBe Some("0")
  // }
  // test("sru ifu1 ra/dec") {
  //   val cfg = GhostConfig(
  //     ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1),
  //     ChannelConfig(GhostBinning.ONE_BY_ONE, 1.seconds, 1),
  //     none,
  //     1.seconds,
  //     FiberAgitator.Off,
  //     FiberAgitator.Off,
  //     "target".some,
  //     coord.some,
  //     none,
  //     none,
  //     none,
  //     none,
  //     none,
  //     none,
  //     Nil
  //   )
  //   println(cfg.map(_.configuration))
  //   cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.type"))   shouldBe Some("1")
  //   cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.bundle")) shouldBe Some("0")
  //   cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.target")) shouldBe Some("2")
  //   // ifu2 not used
  //   cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.type"))   shouldBe Some("1")
  //   cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.target")) shouldBe Some("0")
  // }
}
