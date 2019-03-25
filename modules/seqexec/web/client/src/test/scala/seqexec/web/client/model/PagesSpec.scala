// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
// import monocle.law.discipline.PrismTests
import seqexec.web.client.model.Pages._

/**
  * Tests Client typeclasses
  */
final class PagesSpec extends CatsSuite with ArbitrariesWebClient {

  checkAll("Eq[SeqexecPages]", EqTests[SeqexecPages].eqv)
  checkAll("Eq[StepIdDisplayed]", EqTests[StepIdDisplayed].eqv)
  checkAll("Monoid[StepIdDisplayed]", MonoidTests[StepIdDisplayed].monoid)

  // lenses
  // checkAll("Prism[Action, SeqexecPages]", PrismTests(PageActionP))
}
