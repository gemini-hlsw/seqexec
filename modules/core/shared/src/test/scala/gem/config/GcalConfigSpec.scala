// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.arb._
import gem.config.GcalConfig.GcalArcs
import gem.instances.time._
import gsp.math.arb.ArbTime

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import cats.laws.discipline.arbitrary._
import monocle.law.discipline._


final class GcalConfigSpec extends CatsSuite {

  import ArbCoAdds._
  import ArbEnumerated._
  import ArbGcalConfig._
  import ArbTime._

  checkAll("GcalArcs",   EqTests[GcalArcs].eqv)
  checkAll("GcalConfig", EqTests[GcalConfig].eqv)

  checkAll("GcalArcs.arcs", LensTests(GcalArcs.arcs))

  checkAll("GcalConfig.lamp",         LensTests(GcalConfig.lamp))
  checkAll("GcalConfig.filter",       LensTests(GcalConfig.filter))
  checkAll("GcalConfig.diffuser",     LensTests(GcalConfig.diffuser))
  checkAll("GcalConfig.shutter",      LensTests(GcalConfig.shutter))
  checkAll("GcalConfig.exposureTime", LensTests(GcalConfig.exposureTime))
  checkAll("GcalConfig.coadds",       LensTests(GcalConfig.coadds))
  checkAll("GcalConfig.continuum",    OptionalTests(GcalConfig.continuum))
  checkAll("GcalConfig.arcs",         OptionalTests(GcalConfig.arcs))

}
