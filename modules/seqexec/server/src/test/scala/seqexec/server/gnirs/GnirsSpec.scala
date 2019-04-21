// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import seqexec.server._
import edu.gemini.spModel.gemini.gnirs.GNIRSParams._

/**
  * Tests GNIRS typeclasses
  */
final class GnirsSpec extends CatsSuite with GnirsArbitraries {
  checkAll("Eq[AcquisitionMirror]", EqTests[AcquisitionMirror].eqv)
  checkAll("Eq[WollastonPrism]", EqTests[WollastonPrism].eqv)
  checkAll("Eq[SlitWidth]", EqTests[SlitWidth].eqv)
  checkAll("Eq[CrossDispersed]", EqTests[CrossDispersed].eqv)
  checkAll("Eq[Decker]", EqTests[Decker].eqv)
  checkAll("Eq[Camera]", EqTests[Camera].eqv)
}
