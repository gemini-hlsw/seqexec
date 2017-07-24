// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class UserCheck extends Check {
  import UserDao.Statements._
  "UserDao.Statements" should
            "selectUser"      in check(selectUser(""))
  it should "selectUserʹ"     in check(selectUserʹ("", ""))
  it should "selectRoles"     in check(selectRoles(""))
  it should "changePassword"  in check(changePassword("", "", ""))
}
