// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import java.time.Instant

class EphemerisCheck extends Check {
  import EphemerisDao.Statements._
  "EphemerisDao.Statements" should
            "insert"        in check(insert)
  it should "select"        in check(select(Dummy.ephemerisKey))
  it should "selectBetween" in check(selectBetween(Dummy.ephemerisKey, Instant.now, Instant.now))
}
