// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import org.scalacheck._
import org.scalacheck.Gen._


trait Arbitraries {

  // We can derive Arbitrary[A] given Enumerated[A].
  implicit def arbEnumerated[A](implicit en: Enumerated[A]): Arbitrary[A] =
    Arbitrary(oneOf(en.all))

}
