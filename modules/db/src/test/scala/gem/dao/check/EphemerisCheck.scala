// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.util.InstantMicros

class EphemerisCheck extends Check {
  import EphemerisDao.Statements._
  "EphemerisDao.Statements" should
            "insert"      in check(insert)
  it should "delete"      in check(delete(Dummy.ephemerisKey, Dummy.site))
  it should "select"      in check(select(Dummy.ephemerisKey, Dummy.site))
  it should "selectRange" in check(selectRange(Dummy.ephemerisKey, Dummy.site, InstantMicros.Min, InstantMicros.Max))

  it should "selectNextUserSuppliedKey" in check(selectNextUserSuppliedKey)

  it should "insertMeta"  in check(insertMeta(Dummy.ephemerisKey, Dummy.site, Dummy.ephemerisMeta))
  it should "updateMeta"  in check(updateMeta(Dummy.ephemerisKey, Dummy.site, Dummy.ephemerisMeta))
  it should "deleteMeta"  in check(deleteMeta(Dummy.ephemerisKey, Dummy.site))
  it should "selectMeta"  in check(selectMeta(Dummy.ephemerisKey, Dummy.site))

  it should "selectTimes" in check(selectTimes(Dummy.ephemerisKey, Dummy.site))
}
