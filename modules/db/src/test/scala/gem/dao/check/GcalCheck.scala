// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class GcalCheck extends Check {
  import GcalDao.Statements._
  "GcalDao.Statements" should
            "insertGcal"          in check(insertGcal(0, Dummy.gcalConfig))
  it should "selectGcal"          in check(selectGcal(0))
  it should "selectSmartGcal"     in check(selectSmartGcal(0))
  it should "bulkInsertSmartGcal" in check(bulkInsertSmartGcal)
}
