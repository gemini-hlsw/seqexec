// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.util.InstantMicros

class EphemerisCheck extends Check {
  import EphemerisDao.Statements._
  "EphemerisDao.Statements" should
            "insert"      in check(insert)
  it should "delete"      in check(delete(Dummy.ephemerisKey))
  it should "select"      in check(select(Dummy.ephemerisKey))
  it should "selectRange" in check(selectRange(Dummy.ephemerisKey, InstantMicros.now(), InstantMicros.now()))
}
