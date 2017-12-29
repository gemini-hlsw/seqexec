// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.enum.UserTargetType.Other

class UserTargetCheck extends Check {
  import UserTargetDao.Statements._

  "UserTargetDao.Statements" should
            "insert"    in check(insert(0, Other, Dummy.observationId))
  it should "select"    in check(select(0))
  it should "selectAll" in check(selectAll(Dummy.observationId))

}
