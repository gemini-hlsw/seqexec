// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.enum.ProgramRole

class UserCheck extends Check {
  import UserDao.Statements._
  "UserDao.Statements" should
            "selectUser"      in check(selectUser(""))
  it should "selectUserʹ"     in check(selectUserʹ("", ""))
  it should "selectRoles"     in check(selectRoles(""))
  it should "changePassword"  in check(changePassword("", "", ""))
  it should "setRole"         in check(setRole("", Dummy.programId, ProgramRole.PI))
  it should "unsetRole"       in check(unsetRole("", Dummy.programId, ProgramRole.PI))
  it should "insertUserFlat"  in check(insertUserFlat(Dummy.user, ""))
}
