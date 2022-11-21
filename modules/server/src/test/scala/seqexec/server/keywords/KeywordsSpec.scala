// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import lucuma.core.enums.KeywordName

final class KeywordsSpec extends CatsSuite with KeywordArbitraries {
  checkAll("Eq[KeywordName]", EqTests[KeywordName].eqv)
  checkAll("Eq[KeywordType]", EqTests[KeywordType].eqv)
  checkAll("Eq[InternalKeyword]", EqTests[InternalKeyword].eqv)
  checkAll("Eq[KeywordBag]", EqTests[KeywordBag].eqv)
  checkAll("Monoid[KeywordBag]", MonoidTests[KeywordBag].monoid)
}
