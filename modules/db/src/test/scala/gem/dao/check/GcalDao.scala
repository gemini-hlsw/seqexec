// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class GcalCheck extends Check {
  import GcalDao.Statements._
  "GcalDao.Statements" should
            "insert" in check(insert(Dummy.gcalConfig, None))
  it should "select" in check(select(0))
}
