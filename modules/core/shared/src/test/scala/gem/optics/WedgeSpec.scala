// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package optics

import cats.tests.CatsSuite
import gem.laws.discipline._

final class wedgeSpec extends CatsSuite {

  val se: SplitEpi[Long, Short] =
    SplitEpi(_.toShort, _.toLong)

  val sm: SplitMono[Short, Int] =
    SplitMono(_.toInt, _.toShort)

  val w: Wedge[Long, Int] =
    se composeSplitMono sm

  // Laws
  checkAll("Long > Short", SplitEpiTests(se).splitEpi)
  checkAll("Short < Int",   SplitMonoTests(sm).splitMono)
  checkAll("Long > Short < Int",   WedgeTests(w).wedge)

}
