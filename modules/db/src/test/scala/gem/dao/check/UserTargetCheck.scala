// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.{ Target, UserTarget }
import gem.enum.UserTargetType.Other

class UserTargetCheck extends Check {
  import UserTargetDao.Statements._

  "UserTargetDao.Statements" should
            "insert"           in check(insert(Target.Id(0), Other, Dummy.observationId))
  it should "select"           in check(select(UserTarget.Id(0)))
  it should "selectAllForObs"  in check(selectObs(Dummy.observationId))
  it should "selectAllForProg" in check(selectProg(Dummy.programId))

}
