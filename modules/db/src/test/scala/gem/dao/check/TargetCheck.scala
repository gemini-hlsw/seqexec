// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class TargetCheck extends Check {
  import TargetDao.Statements._
  "TargetDao.Statements" should
            "select" in check(select(0))
  it should "insert" in check(insert(Dummy.target))
  it should "update" in check(update(0, Dummy.target))
  it should "delete" in check(delete(0))
}
