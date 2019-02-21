// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.arb._

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import monocle.law.discipline._
import org.scalacheck.Arbitrary._

import DynamicConfig.{ GmosN, GmosS }

final class DynamicConfigSpec extends CatsSuite {

  import ArbEnumerated._
  import ArbGmos._
  import ArbWavelength._

  checkAll("GmosN", EqTests[GmosN].eqv)

  checkAll("GmosN.common",  LensTests(GmosN.common ))
  checkAll("GmosN.grating", LensTests(GmosN.grating))
  checkAll("GmosN.filter",  LensTests(GmosN.filter))
  checkAll("GmosN.fpu",     LensTests(GmosN.fpu))

  checkAll("GmosN.disperser",  OptionalTests(GmosN.disperser))
  checkAll("GmosN.wavelength", OptionalTests(GmosN.wavelength))
  checkAll("GmosN.builtinFpu", OptionalTests(GmosN.builtinFpu))
  checkAll("GmosN.customMask", OptionalTests(GmosN.customMask))

  checkAll("GmosS", EqTests[GmosS].eqv)

  checkAll("GmosS.common",  LensTests(GmosS.common ))
  checkAll("GmosS.grating", LensTests(GmosS.grating))
  checkAll("GmosS.filter",  LensTests(GmosS.filter))
  checkAll("GmosS.fpu",     LensTests(GmosS.fpu))

  checkAll("GmosS.disperser",  OptionalTests(GmosS.disperser))
  checkAll("GmosS.wavelength", OptionalTests(GmosS.wavelength))
  checkAll("GmosS.builtinFpu", OptionalTests(GmosS.builtinFpu))
  checkAll("GmosS.customMask", OptionalTests(GmosS.customMask))

}
