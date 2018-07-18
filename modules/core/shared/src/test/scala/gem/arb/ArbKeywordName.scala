// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.KeywordName
import org.scalacheck._
import org.scalacheck.Cogen._

trait ArbKeywordName {

  implicit def arbKeywordName: Arbitrary[KeywordName] = Arbitrary {
    Gen.oneOf(KeywordName.all)
  }

  implicit def cogKeywordName: Cogen[KeywordName] =
    Cogen[String].contramap(_.tag)
}

object ArbKeywordName extends ArbKeywordName
