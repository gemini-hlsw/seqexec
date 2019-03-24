// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.Target

class TargetCheck extends Check {
  import TargetDao.Statements._
  "TargetDao.Statements" should
            "select" in check(select(Target.Id(0)))
  it should "insert" in check(insert(Dummy.target))
  it should "update" in check(update(Target.Id(0), Dummy.target))
  it should "delete" in check(delete(Target.Id(0)))
}
