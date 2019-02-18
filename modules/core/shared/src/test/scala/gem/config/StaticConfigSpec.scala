// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.arb._

import cats.tests.CatsSuite
import monocle.law.discipline._
import org.scalacheck.Arbitrary._

import StaticConfig.{ GmosN, GmosS }

final class StaticConfigSpec extends CatsSuite with Arbitraries {

  import ArbEnumerated._

  checkAll("GmosN.common",        LensTests(GmosN.common))
  checkAll("GmosN.stageMode",     LensTests(GmosN.stageMode))
  checkAll("GmosN.customRois",    LensTests(GmosN.customRois))
  checkAll("GmosN.nodAndShuffle", LensTests(GmosN.nodAndShuffle))

  checkAll("GmosS.common",        LensTests(GmosS.common))
  checkAll("GmosS.stageMode",     LensTests(GmosS.stageMode))
  checkAll("GmosS.customRois",    LensTests(GmosS.customRois))
  checkAll("GmosS.nodAndShuffle", LensTests(GmosS.nodAndShuffle))

}
