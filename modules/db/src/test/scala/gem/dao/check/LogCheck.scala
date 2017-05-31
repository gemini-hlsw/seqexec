package gem.dao
package check

import java.util.logging.Level

class LogCheck extends Check {
  import LogDao.Statements._
  "LogDao.Statements" should
            "insert" in check(insert(Dummy.user, Level.INFO, None, "", None, None))
}
