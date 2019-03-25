// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.util.Enumerated
import org.scalacheck._
import org.scalacheck.Gen._

trait ArbEnumerated {

  implicit def arbEnumerated[A](implicit en: Enumerated[A]): Arbitrary[A] =
    Arbitrary(oneOf(en.all))

  implicit def cogEnumerated[A](implicit en: Enumerated[A]): Cogen[A] =
    Cogen[String].contramap(en.tag)

}

object ArbEnumerated extends ArbEnumerated
