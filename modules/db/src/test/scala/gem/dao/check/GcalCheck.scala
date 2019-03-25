// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class GcalCheck extends Check {
  import GcalDao.Statements._
  "GcalDao.Statements" should
            "insertStepGcal"      in check(insertStepGcal(0, Dummy.gcalConfig))
  it should "selectStepGcal"      in check(selectStepGcal(0))
  it should "selectSmartGcal"     in check(selectSmartGcal(0))
  it should "bulkInsertSmartGcal" in check(bulkInsertSmartGcal)
}
