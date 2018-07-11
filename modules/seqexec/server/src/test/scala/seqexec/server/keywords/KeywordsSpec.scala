// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import seqexec.server.SeqexecServerArbitraries._

final class KeywordsSpec extends CatsSuite {
  checkAll("Eq[KeywordType]", EqTests[KeywordType].eqv)
  checkAll("Eq[InternalKeyword]", EqTests[InternalKeyword].eqv)
  checkAll("Eq[KeywordBag]", EqTests[KeywordBag].eqv)
  checkAll("Monoid[KeywordBag]", MonoidTests[KeywordBag].monoid)
}
