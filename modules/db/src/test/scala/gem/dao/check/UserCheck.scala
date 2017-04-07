package gem.dao
package check

class UserCheck extends Check {
  import UserDao.Statements._
  "UserDao.Statements" should
            "select"          in check(select(""))
  it should "selectWithRoles" in check(selectWithRoles("", ""))
  it should "changePassword"  in check(changePassword("", "", ""))
}
