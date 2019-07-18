// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb._
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import monocle.law.discipline._

final class StepSpec extends CatsSuite {

  import ArbEnumerated._
  import ArbGcalConfig._
  import ArbGmos._
  import ArbStep._
  import ArbTelescopeConfig._

  checkAll("Step.Base",           EqTests[Step.Base          ].eqv)
  checkAll("Step.Base.Bias",      EqTests[Step.Base.Bias.type].eqv)
  checkAll("Step.Base.Dark",      EqTests[Step.Base.Dark.type].eqv)
  checkAll("Step.Base.Gcal",      EqTests[Step.Base.Gcal     ].eqv)
  checkAll("Step.Base.Science",   EqTests[Step.Base.Science  ].eqv)
  checkAll("Step.Base.SmartGcal", EqTests[Step.Base.SmartGcal].eqv)

  checkAll("Step.Base.gcalConfig",      PrismTests(Step.Base.gcalConfig))
  checkAll("Step.Base.telescopeConfig", PrismTests(Step.Base.telescopeConfig))
  checkAll("Step.Base.smartGcalType",   PrismTests(Step.Base.smartGcalType))

  checkAll("Step.Base.Gcal.gcal",               LensTests(Step.Base.Gcal.gcal))
  checkAll("Step.Base.Science.telescope",       LensTests(Step.Base.Science.telescope))
  checkAll("Step.Base.SmartGcal.smartGcalType", LensTests(Step.Base.SmartGcal.smartGcalType))

  checkAll("Step.GmosN", EqTests[Step.GmosN].eqv)

  checkAll("Step.GmosN.dynamicConfig", LensTests(Step.GmosN.dynamicConfig))
  checkAll("Step.GmosN.base",          LensTests(Step.GmosN.base))

  checkAll("Step.GmosS", EqTests[Step.GmosS].eqv)

  checkAll("Step.GmosS.dynamicConfig", LensTests(Step.GmosS.dynamicConfig))
  checkAll("Step.GmosS.base",          LensTests(Step.GmosS.base))

}
