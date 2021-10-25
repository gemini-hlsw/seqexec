// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// // Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// // For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
//
// package seqexec.server.ghost
//
// import cats.kernel.laws.discipline._
// import cats.tests.CatsSuite
// import gem.enum.GiapiStatusApply._
// import gsp.math.Coordinates
// import gsp.math.RightAscension
// import gsp.math.Declination
// import org.scalatest.EitherValues
// import scala.concurrent.duration._
//
// /**
//   * Tests GHOST Config typeclasses
//   */
// final class GhostSpec extends CatsSuite with GhostArbitraries with EitherValues {
//   checkAll("Eq[GHOSTConfig]", EqTests[GhostConfig].eqv)
//
//   val dec   = Declination.fromRadians(1.0).getOrElse(Declination.Max)
//   val ra    = RightAscension.fromRadians(2.0)
//   val coord = Coordinates(ra, dec)
//
//   test("fiber agitator on") {
//     val cfg = GhostConfig(none,
//                           1.seconds,
//                           FiberAgitator.On,
//                           FiberAgitator.On,
//                           "target".some,
//                           Coordinates.Zero.some,
//                           none,
//                           none,
//                           none,
//                           none,
//                           none,
//                           none)
//     cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator1.applyItem)) shouldBe Some("1")
//     cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator2.applyItem)) shouldBe Some("1")
//   }
//   test("fiber agitator off") {
//     val cfg = GhostConfig(none,
//                           1.seconds,
//                           FiberAgitator.Off,
//                           FiberAgitator.Off,
//                           "target".some,
//                           Coordinates.Zero.some,
//                           none,
//                           none,
//                           none,
//                           none,
//                           none,
//                           none)
//     cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator1.applyItem)) shouldBe Some("0")
//     cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator2.applyItem)) shouldBe Some("0")
//   }
//   test("sru ifu1 ra/dec") {
//     val cfg = GhostConfig(none,
//                           1.seconds,
//                           FiberAgitator.Off,
//                           FiberAgitator.Off,
//                           "target".some,
//                           coord.some,
//                           none,
//                           none,
//                           none,
//                           none,
//                           none,
//                           none)
//     println(cfg.map(_.configuration))
//     cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.type"))   shouldBe Some("1")
//     cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.bundle")) shouldBe Some("0")
//     cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.target")) shouldBe Some("2")
//     // ifu2 not used
//     cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.type"))   shouldBe Some("1")
//     cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.target")) shouldBe Some("0")
//   }
// }
