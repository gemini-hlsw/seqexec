// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import seqexec.server.gcal.GcalController._

/**
 * Tests Gcal typeclasses
 */
final class GcalSpec extends CatsSuite with GcalArbitraries {
  checkAll("Eq[LampState]", EqTests[LampState].eqv)
  checkAll("Eq[ArLampState]", EqTests[ArLampState].eqv)
  checkAll("Eq[CuArLampState]", EqTests[CuArLampState].eqv)
  checkAll("Eq[QH5WLampState]", EqTests[QH5WLampState].eqv)
  checkAll("Eq[QH100WLampState]", EqTests[QH100WLampState].eqv)
  checkAll("Eq[ThArLampState]", EqTests[ThArLampState].eqv)
  checkAll("Eq[XeLampState]", EqTests[XeLampState].eqv)
  checkAll("Eq[IrLampState]", EqTests[IrLampState].eqv)
}
